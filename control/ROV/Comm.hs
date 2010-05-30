module ROV.Comm (
    Comm(..), Motor(..), newComm, sendMotors
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

import System.IO (hFlush,Handle)

data Comm = Comm {
    commH :: File.Handle,
    commMotors :: M.Map Motor Float
}

data Motor = ML | MR | MV | Pinchers | Pitch
    deriving (Show,Eq,Ord)

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

thrusterByte :: Float -> Word8
thrusterByte x = floor $ 255 * (x + 1) / 2

servoByte :: Float -> Word8
servoByte = floor . (255 *)

sendMotors :: Comm -> IO ()
sendMotors comm@Comm{ commH = fh, commMotors = motors } = do
    hPut fh $ runPut $ do
        putWord8 0x40 -- CMD_SET_MOTORS
        mapM_ (putWord8 . thrusterByte . (motors M.!)) [ML,MR,MV]
        mapM_ (putWord8 . servoByte . (motors M.!)) [Pitch,Pinchers]
    hFlush fh
    res <- runGet getWord8 <$> hGet fh 1
    when (res /= 0x80)
        $ putStrLn "retry" >> sendMotors comm
