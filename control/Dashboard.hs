module Dashboard where

import qualified Graphics.Rendering.OGL as GL
import Graphics.Rendering.OGL (GL,GLfloat,($=),($~))
import qualified Graphics.UI.OGL.GLFW as FW

import qualified Control.Monad as M
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
    M.forever $ do
        FW.pollEvents
        freqAssoc <- readMVar micVar
        temperature <- readMVar tempVar
        GL.runGL (display freqAssoc temperature)
        yield

onKeyDown (FW.SpecialKey FW.ESC) = GL.liftIO $ do
    FW.closeWindow
    FW.terminate
onKeyDown _ = return ()

onKeyUp _ = return ()

display :: Mic.FreqAssoc -> ROV.Temperature -> GL ()
display freqAssoc temperature = do
    GL.clearColor $= GL.Color4 0.7 0.4 0.8 0
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    
    GL.loadIdentity
    
    drawPanel
        (Px 0,Px 0)
        (Percent 50, Percent 100)
        (audioGraph freqAssoc)
    
    drawPanel
        (Percent 60,Px 0)
        (Percent 40, Px 30)
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
    
    GL.preservingMatrix $ do
        GL.color (GL.Color4 1 1 1 1 :: GL.Color4 GLfloat)
        let s = 0.03 :: GLfloat in GL.scale s s s
        FW.renderString FW.Fixed8x16 $ show t

micCoords :: Mic.FreqAssoc -> [(GLfloat,GLfloat)]
micCoords freqAssoc = map f freqAssoc where
    f (freq,amp) = (x,y) where
        x = r $ (freq - minF) / (maxF - minF)
        y = r $ (maxA - amp) / (maxA - minA)
        r :: Double -> GLfloat
        r = fromRational . toRational
    (freqs,amps) = unzip freqAssoc
    (maxF,minF) = (maximum freqs, minimum freqs) -- (5000,1000)
    (maxA,minA) = (5,0)

audioGraph :: Mic.FreqAssoc -> Panel
audioGraph freqAssoc = do
    GL.color (GL.Color3 0.3 0.3 0.3 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Quads $ do
        M.forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
    
    let coords = micCoords freqAssoc
    GL.color (GL.Color3 1 0 0 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Lines
        $ M.forM_ (zip coords $ tail coords) $ \((x0,y0),(x1,y1)) -> do
            GL.vertex $ GL.Vertex2 x0 y0
            GL.vertex $ GL.Vertex2 x1 y1
