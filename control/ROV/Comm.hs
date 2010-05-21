module ROV.Comm (
    Comm(..), Motor(..), newComm, send, isServo, isThruster
) where

import Data.Bits ((.|.),bit)
import Data.Word (Word8)
import qualified Data.Map as M

import qualified System.IO as File
import System.Process (system)

import Data.Binary.Put (runPut,putWord8)
import Data.Binary.Get (runGet,getWord8)
import Data.ByteString.Lazy (ByteString,hPut,hGet)

import Control.Monad (replicateM,when,join)
import Control.Applicative ((<$>))
import System.Random (randomRIO)

import System.IO (hFlush,stdout)

data Comm = Comm {
    commH :: File.Handle,
    commMotors :: M.Map Motor Float
}

data Motor = ML | MR | MV | Pinchers | Pitch
    deriving (Show,Eq,Ord)

isServo Pinchers = True 
isServo Pitch = True
isServo _ = False

isThruster = not . isServo


data Constant = SetThrusters | SetServo Int
    deriving (Show,Eq,Ord)

constant :: Constant -> Word8
constant SetThrusters = 0x40
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
        commMotors = M.fromList $ zip
            [Pinchers,Pitch,ML,MR,MV]
            [0.5,0.5,0,0,0]
    }

send :: Comm -> IO Comm
send comm = do
    rs <- replicateM 3 $ randomRIO (0,1)
    let motors = commMotors comm
        cmds =
            (SetThrusters, motorByte comm rs)
            : (SetServo 0, round $ (*255) $ motors M.! Pitch)
            : (SetServo 1, round $ (*255) $ motors M.! Pinchers)
            : []
    mapM (sendCmd comm) cmds
    print cmds
    hFlush stdout
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
        $ zip [ML,MR,MV] rs
    enoughPower :: (Motor,Float) -> Bool
    enoughPower (m,r) = (abs $ motors M.! m) > r
    
    setBit :: Motor -> Word8
    setBit m = bit $ case (m,(motors M.! m) > 0) of
        (ML,True) -> 2
        (ML,False) -> 3
        (MR,True) -> 0
        (MR,False) -> 1
        (MV,True) -> 5
        (MV,False) -> 4
