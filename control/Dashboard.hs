module Main where

import qualified Graphics.Rendering.OGL as GL
import Graphics.Rendering.OGL (GL,GLfloat,($=),($~))
import qualified Graphics.UI.OGL.GLFW as FW
import qualified Graphics.Rendering.Chart.Simple as Chart

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.List (maximumBy)
import Data.Ord (comparing)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Printf (printf)

import qualified Data.Time.Clock.POSIX as T
import qualified System.Posix.Files (createSymbolicLink,removeLink)
import qualified System.IO as IO

import Control.Concurrent.MVar
import Control.Concurrent (forkOS,yield)
import Control.Applicative ((<$>),(<*>),(<|>))
import Control.Arrow (first,second)

import ROV.Input (getJoystick,readInput)
import ROV.Drive (drive)
import ROV.Interpolate (interpolate,readCalibration,Temp)
import qualified ROV.Mic as Mic
import qualified ROV.Comm as Comm

import Event (setTimeout,setInterval,runEvents)

data TempSample = Base | Mid | Top
    deriving (Ord,Eq,Show)
type SampleMap = M.Map TempSample Temp

sampleHeights :: M.Map TempSample Double
sampleHeights = M.fromList [(Base,40),(Mid,70),(Top,100)]

main :: IO ()
main = do
    js <- getJoystick
    commVar <- newMVar =<< Comm.newComm "/dev/ttyUSB0"
    cal <- readCalibration "data/therm.txt"
    
    FW.initialize
    
    tempVar <- newMVar 0
    tempSampleVar <- newMVar M.empty
    micVar <- newMVar []
    freqMarkVar <- newMVar 0
    
    let markSample :: TempSample -> IO ()
        markSample sample = do
            temp <- readMVar tempVar
            putStrLn $ "Mark at " ++ show sample ++ ": " ++ show temp ++ "° C"
            IO.hFlush IO.stdout
            
            modifyMVar_ tempSampleVar (return . M.insert sample temp)
            forkOS $ do
                samples <- unzip . M.toList . M.mapKeys (sampleHeights M.!)
                    <$> readMVar tempSampleVar
                time <- T.getPOSIXTime
                join $ uncurry (Chart.plotPNG "data/temperature.png") samples
            return ()
    
    FW.openWindow (GL.Size 1024 300) [ FW.DisplayAlphaBits 8 ] FW.Window
    GL.runGL $ do
        GL.shadeModel $= GL.Smooth
        GL.lineSmooth $= GL.Enabled
        GL.blend $= GL.Enabled
        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
        GL.lineWidth $= 2
        (FW.windowSizeCallback $=) $ \size@(GL.Size w h) -> GL.runGL $ do
            GL.viewport $= (GL.Position 0 0, size)
            GL.matrixMode $= GL.Projection
            GL.loadIdentity
            GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
        (FW.keyCallback $=) $ \key state -> case state of
            FW.Press -> case key of
                FW.SpecialKey FW.ESC -> GL.liftIO $ do
                    FW.closeWindow
                    FW.terminate
                FW.CharKey '1' -> GL.liftIO $ markSample Base
                FW.CharKey '2' -> GL.liftIO $ markSample Mid
                FW.CharKey '3' -> GL.liftIO $ markSample Top
                FW.CharKey 'F' -> GL.liftIO $ do
                    micAssoc <- readMVar micVar
                    let freq = round $ fst $ maximumBy (comparing snd)
                            [ (f,a) | (f,a) <- micAssoc, f >= 1000, f <= 5000 ]
                    putStrLn $ "Marked frequency "
                        ++ show freq ++ ": " ++ show micAssoc
                    IO.hFlush IO.stdout
                    
                    swapMVar freqMarkVar freq
                    return ()
                FW.CharKey 'G' -> GL.liftIO $ do
                    samples <- unzip . M.toList . M.mapKeys (sampleHeights M.!)
                        <$> readMVar tempSampleVar
                    forkOS $ join $ uncurry (Chart.plotWindow) samples
                    return ()
                _ -> return ()
            FW.Release -> return ()
    
    runEvents 3
        . setInterval 0 (do 
            comm <- readMVar commVar
            mTemp <- (Just (interpolate cal) <*>) <$> Comm.readTemp comm
            case mTemp of
                Just t -> swapMVar tempVar t >> return ()
                Nothing -> return ()
        )
        . setInterval 0.25 (do
            micAssoc <- Mic.listen "plughw:0,0" (11025 * 2) (1024 * 4)
            swapMVar micVar micAssoc
        )
        . setInterval 0.05 (do
            input <- readInput js
            modifyMVar_ commVar (flip drive $ input)
        )
        $ []
    
    forever $ do
        temp <- readMVar tempVar
        samples <- readMVar tempSampleVar
        micAssoc <- readMVar micVar
        freqMark <- readMVar freqMarkVar
        FW.pollEvents
        GL.runGL (display micAssoc temp samples freqMark)
        yield

display :: Mic.Assoc -> Temp -> SampleMap -> Int -> GL ()
display assoc temperature samples freqMark = do
    GL.clearColor $= GL.Color4 0.7 0.4 0.8 0
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    
    GL.loadIdentity
    
    drawPanel
        (Px 0,Px 0)
        (Percent 50, Percent 100)
        (audioGraph assoc)
    
    drawPanel
        (Percent 50,Px 0)
        (Percent 100, Percent 100)
        (labels temperature samples freqMark)
    
    GL.flush
    FW.swapBuffers

data Coord
    = Percent GLfloat
    | Px GLfloat
type Coords = (Coord,Coord)

-- turn Coords into window coords in (-1,1)
resolve :: Coords -> GL (GLfloat,GLfloat)
resolve (x,y) = do
    GL.Size width height <- GL.get FW.windowSize
    return (f width x,f height y) where
        f _ (Percent c) = (c / 100) * 2 - 1
        f d (Px c) = (c / fromIntegral d) * 2 - 1

-- draw a panel starting at a point with a size
drawPanel :: Coords -> Coords -> Panel -> GL ()
drawPanel pt size panel = GL.preservingMatrix $ do
    (x,y) <- resolve pt
    (w,h) <- resolve size
    let (sw,sh) = (w + 1, h + 1)
    GL.rotate 180 (GL.Vector3 1 0 0 :: GL.Vector3 GLfloat)
    GL.translate $ GL.Vector3 x y 0
    GL.scale sw sh 1
    panel

type Panel = GL ()

renderText :: GLfloat -> String -> GL ()
renderText s' text = GL.preservingMatrix $ do
    GL.Size width height <- GL.get FW.windowSize
    let as = fromIntegral width / fromIntegral height
        s = s' * 0.003
    GL.scale (s / as) (-s) 1
    forM_ (lines text) $ \line -> do
        GL.translate (GL.Vector3 0 (-14) 0 :: GL.Vector3 GLfloat)
        FW.renderString FW.Fixed8x16 line

labels :: Temp -> SampleMap -> Int -> Panel
labels t samples freqMark = do
    GL.color (GL.Color3 0 0 0 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Quads $ do
        forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    GL.color (GL.Color4 1 1 1 1 :: GL.Color4 GLfloat)
    GL.translate (GL.Vector3 0.1 0.1 0 :: GL.Vector3 GLfloat)
    renderText 1
        $ "Temperature: " ++ show (round t) ++ "° C\n\n"
        ++ "    1. Base: " ++ sampleAt Base ++ "° C\n"
        ++ "    2. Mid:  " ++ sampleAt Mid ++ "° C\n"
        ++ "    3. Top:  " ++ sampleAt Top ++ "° C\n"
        ++ "\n\n"
        ++ "Frequency:" ++ show freqMark ++ " Hz\n"
    where
        sampleAt x = fromJust
            $ Just (show . round) <*> M.lookup x samples <|> Just "---"

data MicRange = MicRange Mic.Pair Mic.Pair

micRange :: Mic.Assoc -> MicRange
micRange assoc = MicRange fp ap
    where
        (freqs,amps) = unzip assoc
        fp = (maximum freqs, minimum freqs)
        ap = (maximum amps + 0.5, 0)

micCoord :: MicRange -> Mic.Pair -> (GLfloat,GLfloat)
micCoord (MicRange (maxF,minF) (maxA,minA)) (freq,amp) = (x,y)
    where
        x = r $ (freq - minF) / (maxF - minF)
        y = r $ (maxA - amp) / (maxA - minA)
        r :: Double -> GLfloat
        r = fromRational . toRational

audioGraph :: Mic.Assoc -> Panel
audioGraph assoc = do
    GL.color (GL.Color3 0.3 0.3 0.3 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Quads $ do
        forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    
    let
        range = micRange assoc
        coords = map (micCoord range) assoc
        lines = zip coords (tail coords)
        aoi = map (micCoord range) [ (1000,0), (1000,2), (5000,2), (5000,0) ]
        (bestFreq,bestAmp)
            = first round
            . maximumBy (comparing snd)
            $ [ (f,a) | (f,a) <- assoc, f >= 1000, f <= 5000 ]
    
    when (not $ null assoc) $ do
        GL.color (GL.Color3 0.6 0.3 0.3 :: GL.Color3 GLfloat)
        GL.renderPrimitive GL.Quads $ do
            forM_ aoi $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    
    GL.color (GL.Color3 1 0 0 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Lines $ do
        forM_ lines $ \((x0,y0),(x1,y1)) -> do
            GL.vertex $ GL.Vertex2 x0 y0
            GL.vertex $ GL.Vertex2 x1 y1
    
    when (not $ null assoc) $ GL.preservingMatrix $ do
        GL.color (GL.Color3 1 1 1 :: GL.Color3 GLfloat)
        GL.translate (GL.Vector3 0.1 0.1 0 :: GL.Vector3 GLfloat)
        renderText 2
            $ show bestFreq ++ " Hz\n"
            ++ printf "%.2f" bestAmp
