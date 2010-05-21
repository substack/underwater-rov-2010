module Main where

import ROV

import System.Environment (getArgs)
import qualified Data.Map as M
import Control.Monad.State.Lazy

import Control.Arrow ((&&&))

main :: IO ()
main = mainArgs =<< getArgs

type Argv = [String]

mainArgs :: Argv -> IO ()
mainArgs argv = do
    js <- getJoystick argv
    comm <- newComm "/dev/ttyUSB0"
    runControl js comm handler
    
handler :: InputState -> Comm -> IO Comm
handler state comm = exec comm $ do
    let
        aTup = (M.!) (axes state)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        servoValue = (M.!) (commServos comm)
        button = (M.!) (buttons state)
    
    setMotor MLeft (lx + ly)
    setMotor MRight (-lx + ly)
    setMotor MVertical ry
    setServo SPitch (servoValue SPitch + dy / 8.0)
    setServo SPinchers (servoValue SPinchers + dx / 2.0)
