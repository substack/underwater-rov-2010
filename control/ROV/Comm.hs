{-# LANGUAGE FlexibleInstances #-}
module ROV.Comm (
    Comm(..), Motor(..), newComm, sendMotors
) where

import Data.Word (Word8)
import qualified Data.Map as M

import qualified System.IO as File
import System.Process (system)

import Foreign (Ptr(..),malloc,peek)

import Data.Binary.Put (runPut,putWord8)
import qualified Data.ByteString.Lazy as BS

import Control.Monad (replicateM,when,join,forever)
import Control.Applicative ((<$>))
import Control.Concurrent (forkOS,threadDelay)
import Control.Concurrent.MVar (MVar,newMVar,swapMVar,readMVar)

data Comm = Comm {
    commH :: File.Handle,
    commTempVar :: MVar Word8,
    commMotors :: M.Map Motor Float
} deriving Show

instance Show (MVar Word8) where
    show _ = "<MVar Word8>"

data Motor = ML | MR | MV | Pinchers | Pitch
    deriving (Show,Eq,Ord)

newComm :: FilePath -> IO Comm
newComm dev = do
    system $ "sudo chgrp plugdev /dev/bus/usb -R"
    system $ "sudo chmod g+rw /dev/bus/usb -R"
    system $ "stty raw clocal 57600 cs8 -parenb parodd cstopb -echo < " ++ dev
    fh <- File.openFile dev File.ReadWriteMode
    File.hSetBuffering fh File.NoBuffering
    
    tempVar <- newMVar 0
    let comm = Comm {
        commH = fh,
        commTempVar = tempVar,
        commMotors = M.fromList $ zip
            [Pinchers,Pitch,ML,MR,MV]
            [0.5,0.5,0,0,0]
    }
    commThread comm
    return comm

commThread :: Comm -> IO ()
commThread Comm{ commH = fh, commTempVar = tempVar } = do
    ptr <- malloc :: IO (Ptr Word8)
    forkOS $ forever $ do
        n <- File.hGetBufNonBlocking fh ptr 1
        if n == 1
            then (swapMVar tempVar =<< peek ptr) >> return ()
            else threadDelay $ round $ 10^6 * 0.01
    return ()

thrusterByte :: Float -> Word8
thrusterByte x = floor $ 255 * (x + 1) / 2

servoByte :: Float -> Word8
servoByte = floor . (255 *)

sendMotors :: Comm -> IO ()
sendMotors comm@Comm{ commH = fh, commMotors = motors } = do
    BS.hPut fh $ runPut $ do
        putWord8 0x40 -- CMD_SET_MOTORS
        mapM_ (putWord8 . thrusterByte . (motors M.!)) [ML,MR,MV]
        mapM_ (putWord8 . servoByte . (motors M.!)) [Pitch,Pinchers]
    File.hFlush fh
