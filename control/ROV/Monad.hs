{-# LANGUAGE TypeSynonymInstances #-}
module ROV.Monad (
    evalROV,execROV,runROV,setMotor,
    ($=),($~),($+),($-)
) where

import ROV.Comm
import Data.Word (Word8)

import Control.Monad.State.Lazy (State(..),MonadState(..),modify)
import Control.Applicative ((<$>))
import qualified Data.Map as M

import Control.Monad.Trans
import Control.Concurrent.MVar (readMVar)

type RovM a = State Comm a

evalROV :: Comm -> RovM a -> IO a
evalROV = ((fst <$>) .) . runROV

execROV :: Comm -> RovM a -> IO Comm
execROV = ((snd <$>) .) . runROV

runROV :: Comm -> RovM a -> IO (a,Comm)
runROV comm f = do
    let r@(_,comm') = runState f comm
    sendMotors comm'
    return r

($=) :: Motor -> Float -> RovM ()
($=) = setMotor
infixr 1 $=

($~) :: Motor -> (Float -> Float) -> RovM ()
($~) = modifyMotor
infixr 1 $~

($+) :: Motor -> Float -> RovM ()
motor $+ v = motor $~ (+v)
infixr 1 $+

($-) :: Motor -> Float -> RovM ()
motor $- v = motor $~ subtract v
infixr 1 $-

getMotor :: Motor -> RovM Float
getMotor motor = f <$> get where
    f comm = commMotors comm M.! motor

modifyMotor :: Motor -> (Float -> Float) -> RovM ()
modifyMotor motor f = modify g where
    g comm = comm { commMotors = motors } where
        motors = M.adjust (clamp . f) motor (commMotors comm)
        clamp = if motor `elem` [ML,MR,MV]
            then max (-1) . min 1
            else max 0 . min 1

setMotor :: Motor -> Float -> RovM ()
setMotor motor power = modify f where
    f comm = comm { commMotors = motors } where
        motors = M.insert motor (clamp power) (commMotors comm)
        clamp = if motor `elem` [ML,MR,MV]
            then max (-1) . min 1
            else max 0 . min 1
