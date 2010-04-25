module ROV.Comm (
    Comm(..), Motor(..), Servo(..),
    newComm, setMotor, setServo, send
) where

import Data.Bits ((.|.),bit)
import Data.Word (Word8)
import qualified Data.Map as M

import qualified System.IO as File
import System.Process (system)

import Data.Binary.Put (runPut,putWord8)
import Data.Binary.Get (runGet,getWord8)
import Data.ByteString.Lazy (ByteString,hPut,hGet)

import Control.Monad (replicateM,when)
import Control.Applicative ((<$>))
import System.Random (randomRIO)

data Comm = Comm {
    commH :: File.Handle,
    commMotors :: M.Map Motor Float,
    commServos :: M.Map Servo Float
}

data Motor = MLeft | MRight | MVertical
    deriving (Show,Eq,Ord)

data Servo = SPinchers | SPitch
    deriving (Show,Eq,Ord)

data Constant = SetMotors | SetServo Int

constant :: Constant -> Word8
constant SetMotors = 0x40
constant (SetServo 0) = 0x41
constant (SetServo 1) = 0x42

newComm :: FilePath -> IO Comm
newComm dev = do
    fh <- File.openFile dev File.ReadWriteMode
    system $ "sudo chgrp plugdev /dev/bus/usb -R"
    system $ "sudo chmod g+rw /dev/bus/usb -R"
    system $ "stty raw clocal 57600 cs8 -parenb parodd cstopb -echo < " ++ dev
    return $ Comm {
        commH = fh,
        commServos = M.fromList $ zip [SPinchers,SPitch] (repeat 0.5),
        commMotors = M.fromList $ zip [MLeft,MRight,MVertical] (repeat 0)
    }

setMotor :: Motor -> Float -> Comm -> Comm
setMotor motor power comm =
    comm { commMotors = M.insert motor power (commMotors comm) }

setServo :: Servo -> Float -> Comm -> Comm
setServo servo value comm =
    comm { commServos = M.insert servo value (commServos comm) }

send :: Comm -> IO Comm
send comm = do
    rs <- replicateM 3 $ randomRIO (0,1)
    mapM (sendCmd comm)
        $ (SetMotors, motorByte comm rs)
        : (SetServo 0, round $ (*256) $ (commServos comm M.! SPitch))
        : (SetServo 1, round $ (*256) $ (commServos comm M.!  SPinchers))
        : []
    return comm

sendCmd :: Comm -> (Constant,Word8) -> IO ()
sendCmd comm@Comm{ commH = fh } (c,byte) = do
    hPut fh $ runPut $ do
        putWord8 $ constant c
        putWord8 byte
    res <- runGet getWord8 <$> hGet fh 1
    when (res /= 0x80)
        $ putStrLn "retry" >> sendCmd comm (c,byte)

motorByte :: Comm -> [Float] -> Word8
motorByte Comm{ commMotors = motors } rs = byte where
    byte = foldl (.|.) 0
        $ map (setBit . fst)
        $ filter enoughPower
        $ zip [MLeft,MRight,MVertical] rs
    enoughPower :: (Motor,Float) -> Bool
    enoughPower (m,r) = (abs $ motors M.! m) > r
    
    setBit :: Motor -> Word8
    setBit m = bit $ case (m,(motors M.! m) > 0) of
        (MLeft,True) -> 0
        (MLeft,False) -> 1
        (MRight,True) -> 2
        (MRight,False) -> 3
        (MVertical,True) -> 5
        (MVertical,False) -> 4
