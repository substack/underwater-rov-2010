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
    let la = leftAxis state
    print (magnitude la)
    return $ setMotor comm MLeft (magnitude la)
