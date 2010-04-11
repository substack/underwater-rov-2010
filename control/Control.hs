module ROV.Control where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Joystick as JS

import Control.Monad (when,mapM,liftM2,join)
import Control.Applicative ((<$>),(<*>))
import Control.Arrow ((***),(&&&))

import System.Environment (getArgs)
import Data.Maybe (isNothing,isJust,fromJust,catMaybes)
import Control.Concurrent (forkIO,ThreadId(..))

main :: IO ()
main = mainArgs =<< getArgs

type Argv = [String]

mainArgs :: Argv -> IO ()
mainArgs argv = do
    SDL.init [SDL.InitJoystick]
    js <- getJoystick argv
    print js
    return ()

-- | Select a joystick based on argv or prompted input
getJoystick :: Argv -> IO SDL.Joystick
getJoystick argv = do
    ix <- enumFromTo 0 <$> JS.countAvailable
    names <- mapM JS.tryName ix
    sticks <- mapM JS.tryOpen ix
    
    let avail :: [(String,SDL.Joystick)]
        avail = map (\(x,y) -> (fromJust x, fromJust y))
            $ filter (\(x,y) -> isJust x && isJust y)
            $ zip names sticks
    
    case filter (capable . snd) avail of
        [] -> fail "No capable joysticks availble"
        [(_,js)] -> return js
        xs -> do
            putStrLn "Available capable joysticks:"
            putStr $ unlines $ zipWith
                (\i name -> "    " ++ i ++ ") " ++ name)
                (map show [0..])
                (map fst xs)
            putStr "Joystick number: "
            snd . (xs !!) . read <$> getLine

-- | Joystick has sufficient capabilities to control the ROV
capable :: SDL.Joystick -> Bool
capable js = axes >= 4
    where
        axes = JS.axesAvailable js

-- | 
joystickThread :: SDL.Joystick -> IO ThreadId
joystickThread js = forkIO undefined
