module ROV.Drive (drive) where

import ROV.Input (InputState)
import ROV.Comm (Comm)
import ROV.Mic (listen)

import Control.Monad (when)
import qualified Data.Map as M

drive :: InputState -> Comm -> IO Comm
drive input comm
    = execROV comm $ do
        ML $= lx + ly
        MR $= -lx + ly
        MV $= ry
        when (button Button5) $ Pitch $+ 0.02
        when (button Button4) $ Pitch $- 0.02
        when (button ButtonL) $ Pinchers $+ 0.1
        when (button ButtonR) $ Pinchers $- 0.1
    where 
        aTup = (M.!) (axes input)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        button = (M.!) (buttons input)
