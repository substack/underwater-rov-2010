module Dashboard where

import qualified Graphics.Rendering.OGL as GL
import Graphics.Rendering.OGL (GL,GLfloat,($=),($~))
import qualified Graphics.UI.OGL.GLFW as FW

import qualified Control.Monad as M
import Control.Concurrent.MVar

import qualified Mic

rgbaBits = [ FW.DisplayRGBBits 8 8 8, FW.DisplayAlphaBits 8 ]
depthBits = [ FW.DisplayDepthBits 8 ]

main = do
    micV <- Mic.listen 44100 10000
    
    FW.initialize
    FW.openWindow (GL.Size 1024 300) (rgbaBits ++ depthBits) FW.Window
    GL.runGL $ do
        (FW.windowSizeCallback $=) $ \size -> GL.runGL $ do
            GL.viewport $= (GL.Position 0 0, size)
        (FW.keyCallback $=) $ \key state -> case state of
            FW.Press -> onKeyDown key
            FW.Release -> onKeyUp key
    M.forever $ do
        FW.pollEvents
        freqs <- readMVar micV
        GL.runGL (display freqs)
        FW.sleep 0.001

onKeyDown (FW.SpecialKey FW.ESC) = GL.liftIO $ do
    FW.closeWindow
    FW.terminate
onKeyDown _ = return ()

onKeyUp _ = return ()

display :: Mic.Freqs -> GL ()
display freqs = do
    GL.clearColor $= GL.Color4 0.7 0.4 0.8 0
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    
    GL.loadIdentity
    
    drawPanel (Px 0,Px 0) (Percent 50, Percent 100) (audioGraph freqs)
    
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

audioGraph :: Mic.Freqs -> Panel
audioGraph freqs = do
    GL.color (GL.Color3 0.3 0.3 0.3 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Quads $ do
        M.forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
