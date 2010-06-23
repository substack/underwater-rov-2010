module ROV.Drive (drive) where

import ROV.Monad
import ROV.Comm
import ROV.Input

import Control.Monad (when)
import qualified Data.Map as M

drive :: Comm -> InputState -> IO Comm
drive comm input
    = execROV comm $ do
        let slx = if lx > 0 then 1 else -1
            sly = if ly > 0 then 1 else -1
            sry = if ry > 0 then 1 else -1
        ML $= slx * (negate $ lx ** 2) + sly * (ly ** 2)
        MR $= slx * (lx ** 2) + sly * (ly ** 2)
        MV $= sry * (ry ** 2)
        when (button Button5) $ Pitch $+ 0.1
        when (button Button4) $ Pitch $- 0.1
        when (button ButtonL) $ Pinchers $+ 0.25
        when (button ButtonR) $ Pinchers $- 0.25
    where 
        aTup = (M.!) (axes input)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        button = (M.!) (buttons input)
