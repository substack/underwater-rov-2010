module Main where

import Sound.Alsa
import Numeric.Transform.Fourier.FFT (fft)
import qualified Data.Array as Ax

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Monad (forever)
import Data.Complex (Complex(..),magnitude)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

import Data.List (sortBy)
import Data.Function (on)

type MicState = (Double,Double) -- frequency, volume

main :: IO ()
main = do
    micV <- micThread 44100 10000
    forever $ do
        (rawVol, freq) <- readMVar micV
        print (rawVol,freq)
        threadDelay $ 5 * 10 ^ 5

micThread :: Int -> Int -> IO (MVar MicState)
micThread sampleRate samples = do
    let
        source = alsaSoundSource "plughw:0,0" soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    mv <- newMVar (0,0)
    forkIO $ withSoundSource source $ \handle -> forever $ do
        takeMVar mv
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        
        let
            volume = (sum $ map abs rawSound) / fromIntegral n
            rawFreqs = map magnitude $ Ax.elems $ fft
                $ Ax.listArray (0,n-1) $ map (:+ 0) rawSound
            sampleStep = (fromIntegral sampleRate / fromIntegral samples)
            freqs = takeWhile ((< 5000) . fst)
                $ dropWhile ((< 1000) . fst)
                $ zip (iterate (+ sampleStep) 0) rawFreqs
        
        print $ take 20 $ reverse $ sortBy (compare `on` snd) freqs
        
        putMVar mv (0,0)
        -- putMVar mv $ if isNaN hz then (0,0) else (hz,volume)
        return ()
    return mv
