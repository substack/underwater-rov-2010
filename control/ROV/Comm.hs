module ROV.Comm (
    Comm(..), newComm, setMotor, send
) where

import Data.Bits ((.|.))
import Data.Word (Word8)
import qualified Data.Map as M

import qualified System.IO as File
import System.Process (system)

import Data.Binary.Put (runPut,putWord8)
import Data.ByteString.Lazy (ByteString,hPut,hGet)

import Control.Concurrent (MVar,newMVar,takeMVar,putMVar)

data Comm = Comm {
    commH :: File.Handle,
    commT :: MVar Int,
    commMotors :: M.Map Motor Float
}

data Motor = MLeft | MRight | MVertical
    deriving (Show,Eq,Ord)

newComm :: FilePath -> IO Comm
newComm dev = do
    fh <- File.openFile dev File.ReadWriteMode
    system $ "stty raw clocal 57600 cs8 -parenb parodd cstopb -echo < " ++ dev
    tVar <- newMVar 0
    return $ Comm {
        commT = tVar,
        commH = fh,
        commMotors = M.fromList $ zip [MLeft,MRight,MVertical] (repeat 0)
    }

setMotor :: Comm -> Motor -> Float -> Comm
setMotor comm motor power =
    comm { commMotors = M.insert motor power (commMotors comm) }

send :: Comm -> IO ()
send comm@Comm{ commT = tVar, commH = fh } = do
    t <- takeMVar tVar
    hPut fh $ runPut (putWord8 $ motorByte comm t)
    putMVar tVar (t + 1)

motorByte :: Comm -> Int -> Word8
motorByte comm t = byte where
    byte = undefined
