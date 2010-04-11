module ROV.Comm (
    Comm(..), newComm, setMotor, send
) where

import Data.Bits ((.|.),bit)
import Data.Word (Word8)
import qualified Data.Map as M

import qualified System.IO as File
import System.Process (system)

import Data.Binary.Put (runPut,putWord8)
import Data.ByteString.Lazy (ByteString,hPut,hGet)

import Control.Monad (replicateM)
import System.Random (randomRIO)

data Comm = Comm {
    commH :: File.Handle,
    commMotors :: M.Map Motor Float
}

data Motor = MLeft | MRight | MVertical
    deriving (Show,Eq,Ord)

newComm :: FilePath -> IO Comm
newComm dev = do
    fh <- File.openFile dev File.ReadWriteMode
    system $ "stty raw clocal 57600 cs8 -parenb parodd cstopb -echo < " ++ dev
    return $ Comm {
        commH = fh,
        commMotors = M.fromList $ zip [MLeft,MRight,MVertical] (repeat 0)
    }

setMotor :: Comm -> Motor -> Float -> Comm
setMotor comm motor power =
    comm { commMotors = M.insert motor power (commMotors comm) }

send :: Comm -> IO ()
send comm@Comm{ commH = fh } = do
    rs <- replicateM 3 $ randomRIO (0,1)
    hPut fh $ runPut (putWord8 $ motorByte comm rs)

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
