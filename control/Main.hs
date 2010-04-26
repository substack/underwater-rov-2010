module Main where

import ROV.Control
import ROV.Comm
import System.Environment (getArgs)

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
    let ((lx,ly),(rx,ry)) = leftAxis &&& rightAxis $ state
    send
        $ setMotor MLeft (lx + ly)
        $ setMotor MRight (-lx + ly)
        $ setMotor MVertical ry
        $ setServo SPinchers ((ry + 1) / 2)
        $ setServo SPitch ((lx + 1) / 2)
        $ comm
