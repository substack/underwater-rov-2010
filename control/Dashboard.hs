module Main where

import qualified Graphics.Rendering.OGL as GL
import Graphics.Rendering.OGL (GL,GLfloat,($=),($~))
import qualified Graphics.UI.OGL.GLFW as FW

import qualified Control.Monad as M
import Control.Monad.Trans (liftIO)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Control.Concurrent.MVar
import Control.Concurrent (yield)
import System.Environment (getArgs)

import qualified Mic
import qualified ROV

main :: IO ()
main = do
    argv <- getArgs
    
    micVar <- Mic.listen "plughw:0,0" (11025 * 2) 4000
    tempVar <- ROV.drive "/dev/ttyUSB0"
    
    FW.initialize
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
            FW.Press -> onKeyDown key
            FW.Release -> onKeyUp key
    
    tempsVar <- newMVar []
    runningTempVar <- newMVar 0
    
    M.forever $ do
        FW.pollEvents
        assoc <- readMVar micVar
        temperature <- readMVar tempVar
        
        modifyMVar_ tempsVar (\t -> return $ temperature : t)
        temps <- readMVar tempsVar
        M.when (length temps >= 10) $ do
            swapMVar tempsVar []
            let avg = sum temps / (fromIntegral $ length temps)
            swapMVar runningTempVar avg
            return ()
        
        runningTemp <- readMVar runningTempVar
        GL.runGL (display assoc runningTemp)

onKeyDown (FW.SpecialKey FW.ESC) = GL.liftIO $ do
    FW.closeWindow
    FW.terminate
onKeyDown _ = return ()

onKeyUp _ = return ()

display :: Mic.Assoc -> ROV.Temperature -> GL ()
display assoc temperature = do
    GL.clearColor $= GL.Color4 0.7 0.4 0.8 0
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    
    GL.loadIdentity
    
    drawPanel
        (Px 0,Px 0)
        (Percent 50, Percent 100)
        (audioGraph assoc)
    
    drawPanel
        (Percent 50,Px 0)
        (Percent 50, Px 20)
        (temperatureLabel temperature)
    
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

temperatureLabel :: ROV.Temperature -> Panel
temperatureLabel t = do
    GL.color (GL.Color3 0 0 0 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Quads $ do
        M.forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    
    GL.color (GL.Color4 1 1 1 1 :: GL.Color4 GLfloat)
    renderText 0.01 $ show (round t) ++ "° C"

renderText :: GLfloat -> String -> GL ()
renderText s text = GL.preservingMatrix $ do
    GL.rotate 180 (GL.Vector3 1 0 0 :: GL.Vector3 GLfloat)
    GL.translate (GL.Vector3 0 (-1) 0 :: GL.Vector3 GLfloat)
    GL.Size width height <- GL.get FW.windowSize
    let as = fromIntegral width / fromIntegral height
    GL.scale s (s * as * 4) 0
    FW.renderString FW.Fixed8x16 text

data MicRange = MicRange Mic.Pair Mic.Pair

micRange :: Mic.Assoc -> MicRange
micRange assoc = MicRange fp ap
    where
        (freqs,amps) = unzip assoc
        fp = (maximum freqs, minimum freqs)
        ap = (2,0)

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
        M.forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    
    let
        range = micRange assoc
        coords = map (micCoord range) assoc
        lines = zip coords (tail coords)
        aoi = map (micCoord range) [ (1000,0), (1000,2), (5000,2), (5000,0) ]
        best = maximumBy (comparing snd)
            [ (f,a) | (f,a) <- assoc, f >= 1000, f <= 5000 ]
    
    M.when (not $ null assoc) $ do
        GL.color (GL.Color3 0.6 0.3 0.3 :: GL.Color3 GLfloat)
        GL.renderPrimitive GL.Quads $ do
            M.forM_ aoi $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    
    GL.color (GL.Color3 1 0 0 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Lines $ do
        M.forM_ lines $ \((x0,y0),(x1,y1)) -> do
            GL.vertex $ GL.Vertex2 x0 y0
            GL.vertex $ GL.Vertex2 x1 y1
    
    M.when (not $ null assoc) $ GL.preservingMatrix $ do
        GL.color (GL.Color3 1 1 1 :: GL.Color3 GLfloat)
        GL.translate (GL.Vector3 0 (-0.5) 0 :: GL.Vector3 GLfloat)
        renderText 0.003 $ show $ round $ fst best
