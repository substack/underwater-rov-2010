module Mic (
    FreqAssoc(..), listen
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
type FreqAssoc = [(Frequency,Amplitude)]
type Device = String

listen :: Device -> Int -> Int -> IO (MVar FreqAssoc)
listen dev sampleRate samples = do
    let
        source = alsaSoundSource dev soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    mv <- newMVar []
    thId <- forkIO $ withSoundSource source $ \handle -> forever $ do
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        
        let
            amps = map magnitude $ Ax.elems $ fft
                $ Ax.listArray (0,n-1) $ map (:+ 0) rawSound
            sampleStep = (fromIntegral sampleRate / fromIntegral n)
            freqs = takeWhile (< 5000) $ dropWhile (< 1000)
                $ iterate (+ sampleStep) 0
            freqAssoc = zip freqs amps
        
        swapMVar mv freqAssoc
        yield
    return mv
