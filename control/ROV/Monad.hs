module ROV.Monad (
    evalROV,execROV,runROV,setMotor,
    (@=),(@:),(@+),(@-)
) where

import ROV.Comm

import Control.Monad.State.Lazy (State(..),MonadState(..),modify)
import Control.Applicative ((<$>))
import qualified Data.Map as M

type ROV a = State Comm a

evalROV :: Comm -> ROV a -> IO a
evalROV = ((fst <$>) .) . runROV

execROV :: Comm -> ROV a -> IO Comm
execROV = ((snd <$>) .) . runROV

runROV :: Comm -> ROV a -> IO (a,Comm)
runROV comm f = do
    let r@(value,comm') = runState f comm
    send comm'
    return r

(@=) :: Motor -> Float -> ROV ()
(@=) = setMotor
infixr 1 @=

(@:) :: Motor -> (Float -> Float) -> ROV ()
(@:) = modifyMotor
infixr 1 @:

(@+) :: Motor -> Float -> ROV ()
motor @+ v = motor @: (+v)
infixr 1 @+

(@-) :: Motor -> Float -> ROV ()
motor @- v = motor @: subtract v
infixr 1 @-

getMotor :: Motor -> ROV Float
getMotor motor = f <$> get where
    f comm = commMotors comm M.! motor

modifyMotor :: Motor -> (Float -> Float) -> ROV ()
modifyMotor motor f = modify g where
    g comm = comm { commMotors = motors } where
        motors = M.adjust (clamp . f) motor (commMotors comm)
        clamp = if isThruster motor
            then max (-1) . min 1
            else max 0 . min 1

setMotor :: Motor -> Float -> ROV ()
setMotor motor power = modify f where
    f comm = comm { commMotors = motors } where
        motors = M.insert motor (clamp power) (commMotors comm)
        clamp = if isThruster motor
            then max (-1) . min 1
            else max 0 . min 1
