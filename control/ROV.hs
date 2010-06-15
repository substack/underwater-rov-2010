module ROV (
    Temperature, drive
) where

import ROV.Control
import ROV.Comm
import ROV.Monad

import qualified Data.Map as M
import Data.Word (Word8)

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar

type Device = String
type Temperature = Double

drive :: Device -> IO (MVar Temperature)
drive dev = do
    tempV <- newMVar 0
    js <- getJoystick
    comm <- newComm dev
    forkOS $ runControl js comm (handler tempV)
    return tempV
 
handler :: MVar Temperature -> InputState -> Comm -> IO Comm
handler tempV state comm = do
    let
        aTup = (M.!) (axes state)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        button = (M.!) (buttons state)
    (comm',t) <- execROV comm $ do
        ML $= lx + ly
        MR $= -lx + ly
        MV $= -ry
        Pitch $+ dy / 64.0
        Pinchers $+ dx / 10.0
        getTemp
    swapMVar tempV $ interpolate t
    return comm'

interpolate :: Word8 -> Temperature
interpolate = fromIntegral -- for now
