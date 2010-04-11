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
    run js (handler comm)
    
handler :: Comm -> InputState -> IO ()
handler comm state = do
    print $ angle &&& magnitude $ leftAxis state
