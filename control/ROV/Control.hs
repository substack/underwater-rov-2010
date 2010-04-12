module ROV.Control (
    InputState(..), AxisState,
    getJoystick, run, angle, magnitude,
) where

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Joystick as JS

import Data.Int (Int16)

import Control.Monad (mapM,forever)
import Control.Applicative ((<$>))

import Data.Maybe (isNothing,isJust,fromJust)
import Control.Concurrent (forkIO,MVar,newMVar,swapMVar,readMVar)

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
magnitude (x,y) = dist (x,y) * scale
    where
        dist (x,y) = sqrt $ x ** 2 + y ** 2
        scale = dist ((cos a)**2, (sin a)**2)
        a = angle (abs x, abs y)

data InputState = InputState {
    leftAxis :: AxisState,
    rightAxis :: AxisState
} deriving Show

joystickThread :: SDL.Joystick -> IO (MVar InputState)
joystickThread js = do
    var <- newMVar $ InputState {
            leftAxis = (0,0),
            rightAxis = (0,0)
        }
    forkIO $ forever $ do
        JS.update
        let mb = fromIntegral (maxBound :: Int16) :: Float
        [lx,ly,rx,ry] <- mapM (((/mb) . fromIntegral <$>) . JS.getAxis js) [0,1,3,2]
        swapMVar var $ InputState {
                leftAxis = (lx,-ly),
                rightAxis = (rx,-ry)
            }
        return ()
    return var

run :: SDL.Joystick -> a -> (InputState -> a -> IO a) -> IO ()
run js x f = do
    var <- joystickThread js
    (flip iterateM_ $ x) $ \x' -> do
        state <- readMVar var
        f state x'

iterateM_ :: (Functor m, Monad m) => (a -> m a) -> a -> m ()
iterateM_ f x = iterateM_ f =<< f x
