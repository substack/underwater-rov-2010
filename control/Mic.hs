module Mic (
    Freqs(..), listen
) where

import Sound.Alsa
import Numeric.Transform.Fourier.FFT (fft)
import qualified Data.Array as Ax

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Monad (forever)
import Data.Complex (Complex(..),magnitude)

import Control.Concurrent (forkIO,yield)
import Control.Concurrent.MVar

type Frequency = Double
type Amplitude = Double
type Freqs = [(Frequency,Amplitude)]

listen :: Int -> Int -> IO (MVar Freqs)
listen sampleRate samples = do
    let
        source = alsaSoundSource "plughw:0,0" soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    mv <- newMVar []
    thId <- forkIO $ withSoundSource source $ \handle -> forever $ do
        takeMVar mv
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        
        let
            amplitudes = map magnitude $ Ax.elems $ fft
                $ Ax.listArray (0,n-1) $ map (:+ 0) rawSound
            sampleStep = (fromIntegral sampleRate / fromIntegral samples)
            freqAssoc = takeWhile ((< 5000) . fst)
                $ dropWhile ((< 1000) . fst)
                $ zip (iterate (+ sampleStep) 0) amplitudes
        
        putMVar mv freqAssoc
        yield
    return mv
