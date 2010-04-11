module ROV.Control (
    InputState(..), AxisState,
    getJoystick, run, angle, magnitude,
) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Joystick as JS

import Data.Int (Int16)

import Control.Monad (mapM,join,liftM2)
import Control.Applicative ((<$>))

import Data.Maybe (isNothing,isJust,fromJust)
import Control.Concurrent (MVar,newMVar,takeMVar,putMVar)

type Argv = [String]

-- | Select a joystick based on argv or prompted input
getJoystick :: Argv -> IO SDL.Joystick
getJoystick argv = do
    SDL.init [SDL.InitJoystick]
    
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

run :: SDL.Joystick -> a -> (InputState -> a -> IO a) -> IO ()
run js x f = (flip iterateM_ $ x) $ \x' -> do
    (flip f $ x') =<< getState js

iterateM_ :: (Functor m, Monad m) => (a -> m a) -> a -> m ()
iterateM_ f x = iterateM_ f =<< f x
