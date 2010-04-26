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
    print state
    return comm
    {-
    send
        $ setMotor MLeft (lx + ly)
        $ setMotor MRight (-lx + ly)
        $ setMotor MVertical ry
        $ setServo SPitch ((1 - ry) / 2)
        $ setServo SPinchers ((lx + 1) / 2)
        $ comm
    -}
