module ROV.Drive (drive) where

import ROV.Monad
import ROV.Comm
import ROV.Input

import Control.Monad (when)
import qualified Data.Map as M

drive :: Comm -> InputState -> IO Comm
drive comm input
    = execROV comm $ do
        ML $= lx + ly
        MR $= -lx + ly
        MV $= ry
        when (button Button5) $ Pitch $+ 0.25
        when (button Button4) $ Pitch $- 0.25
        when (button ButtonL) $ Pinchers $+ 0.25
        when (button ButtonR) $ Pinchers $- 0.25
    where 
        aTup = (M.!) (axes input)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        button = (M.!) (buttons input)
