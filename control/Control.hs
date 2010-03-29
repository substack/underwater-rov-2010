module ROV.Control where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Joystick as JS

import Control.Monad (when,forM_)
import Control.Applicative ((<$>))

import System.Environment (getArgs)
import Data.Maybe (isNothing,fromJust)

main :: IO ()
main = do
    SDL.init [SDL.InitJoystick]
    avail <- JS.countAvailable
    when (avail == 0) $ fail "No joysticks available"
    
    mJs <- (JS.tryOpen =<<) . (pred <$>) . (=<< getArgs)
        $ \argv -> case argv of
            [] -> do
                putStrLn "Available joysticks:"
                forM_ [1..avail] $ \n ->
                    putStrLn . (("    " ++ show n ++ ") ") ++) =<< JS.name (n - 1)
                putStr "Joystick number: "
                read <$> getLine
            (n:_) -> return $ read n
    
    when (isNothing mJs) $ fail "Selected joystick not available"
    let js = fromJust mJs
    print js
