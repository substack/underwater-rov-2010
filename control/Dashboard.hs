module Dashboard where

import qualified Graphics.Rendering.OGL as GL
import Graphics.Rendering.OGL (GL,($=),($~))
import qualified Graphics.UI.OGL.GLFW as FW

import Control.Monad (forever)

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
    GL.flush
    FW.swapBuffers
