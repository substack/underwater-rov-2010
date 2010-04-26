module Main where

import ROV.Control
import ROV.Comm
import System.Environment (getArgs)
import qualified Data.Map as M

import Control.Arrow ((&&&))

main :: IO ()
main = mainArgs =<< getArgs

type Argv = [String]

mainArgs :: Argv -> IO ()
mainArgs argv = do
    js <- getJoystick argv
    comm <- newComm "/dev/ttyUSB0"
    run js comm handler
    
handler :: InputState -> Comm -> IO Comm
handler state comm = do
    let (lx,ly) = (axes state) M.! LeftAxis
        (rx,ry) = (axes state) M.! RightAxis
        (dx,dy) = (axes state) M.! DPad
    send
        $ setMotor MLeft (lx + ly)
        $ setMotor MRight (-lx + ly)
        $ setMotor MVertical ry
        $ setServo SPitch ((1 - dy) / 2)
        $ setServo SPinchers ((dx + 1) / 2)
        $ comm
