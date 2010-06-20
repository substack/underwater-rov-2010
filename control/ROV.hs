module ROV (
    Temperature, drive
) where

import ROV.Control
import ROV.Comm
import ROV.Monad
import Interpolate (Calibration,readCalibration,interpolate)

import Control.Monad (when)

import qualified Data.Map as M
import Data.Word (Word8)

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar

type Device = String
type Temperature = Double

drive :: Device -> IO (MVar Temperature)
drive dev = do
    tempV <- newMVar 0
    cal <- readCalibration
    js <- getJoystick
    comm <- newComm dev
    forkOS $ runControl js comm (handler cal tempV)
    return tempV
 
handler :: Calibration -> MVar Temperature -> InputState -> Comm -> IO Comm
handler cal tempV state comm = do
    let
        aTup = (M.!) (axes state)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        button = (M.!) (buttons state)
    (comm',t) <- execROV comm $ do
        ML $= lx + ly
        MR $= -lx + ly
        MV $= ry
        when (button Button5) $ Pitch $+ 0.02
        when (button Button4) $ Pitch $- 0.02
        when (button ButtonL) $ Pinchers $+ 0.1
        when (button ButtonR) $ Pinchers $- 0.1
        getTemp
    swapMVar tempV $ interpolate cal t
    return comm'
