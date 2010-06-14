module Dashboard where

import qualified Graphics.Rendering.OGL as GL
import Graphics.Rendering.OGL (GL,GLfloat,($=),($~))
import qualified Graphics.UI.OGL.GLFW as FW

import Control.Monad (forever,forM_,mapM_,when,unless)

rgbaBits = [ FW.DisplayRGBBits 8 8 8, FW.DisplayAlphaBits 8 ]
depthBits = [ FW.DisplayDepthBits 8 ]

main = do
    FW.initialize
    FW.openWindow (GL.Size 1024 300) (rgbaBits ++ depthBits) FW.Window
    forever $ do
        FW.pollEvents
        GL.runGL display

display :: GL ()
display = do
    GL.clear [ GL.ColorBuffer, GL.DepthBuffer ]
    GL.clearColor $= GL.Color4 0.7 0.4 0.8 0
    
    drawPanel (Px 0,Px 0) (Percent 100, Percent 100) audioGraph
    
    GL.flush
    FW.swapBuffers

data Coord
    = Percent GLfloat
    | Px GLfloat
type Coords = (Coord,Coord)

-- translate (-1,1) coords 
drawPanel :: Coords -> Coords -> Panel -> GL ()
drawPanel pt1 pt2 panel = return ()

type Panel = GL ()

audioGraph :: Panel
audioGraph = do
    GL.color (GL.Color3 1 0 0 :: GL.Color3 GLfloat)
    GL.renderPrimitive GL.Quads $ do
        forM_ ([(0,0),(0,1),(1,1),(1,0)] :: [(GLfloat,GLfloat)])
            $ \(x,y) -> GL.vertex $ GL.Vertex2 x y
