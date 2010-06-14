{-# LANGUAGE TypeSynonymInstances #-}
module ROV.Monad (
    evalROV,execROV,runROV,setMotor, getTemp,
    ($=),($~),($+),($-)
) where

import ROV.Comm
import Data.Word (Word8)

import Control.Monad.State.Lazy (State(..),MonadState(..),modify)
import Control.Applicative ((<$>))
import qualified Data.Map as M

import Control.Monad.Trans
import Control.Concurrent.MVar (readMVar)

type ROV a = State CommState a
type CommState = (Comm,Word8)

evalROV :: Comm -> ROV a -> IO a
evalROV = ((fst <$>) .) . runROV

execROV :: Comm -> ROV a -> IO CommState
execROV = ((snd <$>) .) . runROV

runROV :: Comm -> ROV a -> IO (a,CommState)
runROV comm f = do
    t <- readMVar $ commTempVar comm
    let r@(value,(comm',t')) = runState f (comm,t)
    sendMotors comm'
    print comm'
    return r

($=) :: Motor -> Float -> ROV ()
($=) = setMotor
infixr 1 $=

($~) :: Motor -> (Float -> Float) -> ROV ()
($~) = modifyMotor
infixr 1 $~

($+) :: Motor -> Float -> ROV ()
motor $+ v = motor $~ (+v)
infixr 1 $+

($-) :: Motor -> Float -> ROV ()
motor $- v = motor $~ subtract v
infixr 1 $-

getMotor :: Motor -> ROV Float
getMotor motor = f <$> get where
    f (comm,_) = commMotors comm M.! motor

modifyMotor :: Motor -> (Float -> Float) -> ROV ()
modifyMotor motor f = modify g where
    g (comm,t) = (comm { commMotors = motors },t) where
        motors = M.adjust (clamp . f) motor (commMotors comm)
        clamp = if motor `elem` [ML,MR,MV]
            then max (-1) . min 1
            else max 0 . min 1

setMotor :: Motor -> Float -> ROV ()
setMotor motor power = modify f where
    f (comm,t) = (comm { commMotors = motors },t) where
        motors = M.insert motor (clamp power) (commMotors comm)
        clamp = if motor `elem` [ML,MR,MV]
            then max (-1) . min 1
            else max 0 . min 1

getTemp :: ROV Word8
getTemp = snd <$> get
