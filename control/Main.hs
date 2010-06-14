module Main where

import ROV

import System.Environment (getArgs)
import qualified Data.Map as M
import Control.Monad.State.Lazy
import Control.Arrow ((&&&))
import Control.Monad.Trans (liftIO)

main :: IO ()
main = mainArgs =<< getArgs

type Argv = [String]

mainArgs :: Argv -> IO ()
mainArgs argv = do
    js <- getJoystick argv
    comm <- newComm "/dev/ttyUSB0"
    runControl js comm handler
    
handler :: InputState -> Comm -> IO Comm
handler state comm = do
    let
        aTup = (M.!) (axes state)
        (lx,ly) = aTup LeftAxis
        (rx,ry) = aTup RightAxis
        (dx,dy) = aTup DPad
        button = (M.!) (buttons state)
    (comm',t) <- execROV comm $ do
        ML $= -lx + ly
        MR $= lx + ly
        MV $= -ry
        Pitch $+ dy / 64.0
        Pinchers $+ dx / 10.0
        getTemp
    print t
    return comm'
