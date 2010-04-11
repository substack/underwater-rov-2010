module ROV.Control where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Joystick as JS

import Control.Monad (when,forM_)
import Control.Applicative ((<$>))

import System.Environment (getArgs)
import Data.Maybe (isNothing,fromJust)
import Control.Concurrent (forkIO,ThreadId(..))

main :: IO ()
main = mainArgs =<< getArgs

type Argv = [String]

mainArgs :: Argv -> IO ()
mainArgs argv = do
    SDL.init [SDL.InitJoystick]
    js <- getJoystick argv
    return ()

-- | Select a joystick based on argv or prompted input
getJoystick :: Argv -> IO SDL.Joystick
getJoystick argv = do
    avail <- JS.countAvailable
    when (avail == 0) $ fail "No joysticks available"
    mJs <- (JS.tryOpen =<<) . (pred <$>)
        $ case argv of
            [] -> do
                putStrLn "Available joysticks:"
                forM_ [1..avail] $ \n ->
                    putStrLn . (("    " ++ show n ++ ") ") ++) =<< JS.name (n - 1)
                putStr "Joystick number: "
                read <$> getLine
            (n:_) -> return $ read n
    
    when (isNothing mJs) $ fail "Selected joystick not available"
    return $ fromJust mJs

-- | Joystick has sufficient capabilities to control the ROV
capable :: SDL.Joystick -> IO Bool
capable js = do
    print $ JS.axesAvailable js
    return False

-- | 
joystickThread :: SDL.Joystick -> IO ThreadId
joystickThread js = forkIO undefined
