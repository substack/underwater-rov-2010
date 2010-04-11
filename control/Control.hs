module ROV.Control where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Joystick as JS

import Data.Int (Int16)

import Control.Monad (forever,mapM)
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
    run js

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

-- | Sufficient capabilities to control the ROV
capable :: SDL.Joystick -> Bool
capable js = axes >= 4
    where axes = JS.axesAvailable js

type AxisState = (Float,Float)

angle :: AxisState -> Float
angle (x,y) = atan2 y x

magnitude :: AxisState -> Float
magnitude (x,y) = sqrt $ x ** 2 + y ** 2

data InputState = InputState {
    leftAxis :: AxisState,
    rightAxis :: AxisState
} deriving Show

getState :: SDL.Joystick -> IO InputState
getState js = do
    JS.update
    
    let mb = fromIntegral (maxBound :: Int16) :: Float
    [lx,ly,rx,ry] <- mapM (((/mb) . fromIntegral <$>) . JS.getAxis js) [0,1,3,2]
    
    return $ InputState {
        leftAxis = (lx,-ly),
        rightAxis = (rx,-ry)
    }

run :: SDL.Joystick -> IO ()
run js = forever $ do
    state <- getState js
    print $ angle &&& magnitude $ leftAxis state
