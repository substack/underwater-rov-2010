module Mic (
    Assoc(..), Pair, Frequency, Amplitude, listen
) where

import Sound.Alsa
import Numeric.Transform.Fourier.FFT (fft)
import qualified Data.Array as Ax

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Monad (forever)
import Data.Complex (Complex(..),magnitude)

import Control.Concurrent (forkOS,yield)
import Control.Concurrent.MVar

type Frequency = Double
type Amplitude = Double
type Pair = (Frequency,Amplitude)
type Assoc = [Pair]
type Device = String

listen :: Device -> Int -> Int -> IO (MVar Assoc)
listen dev sampleRate samples = do
    let
        source = alsaSoundSource dev soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    mv <- newMVar []
    thId <- forkOS $ withSoundSource source $ \handle -> forever $ do
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        
        let
            amps = map magnitude $ Ax.elems $ fft
                $ Ax.listArray (0,n-1) $ map (:+ 0) rawSound
            sampleStep = fromIntegral sampleRate / fromIntegral n
            freqs = [ 0, sampleStep .. fromIntegral sampleRate / 2 ]
            freqAssoc = zip freqs amps
        
        swapMVar mv freqAssoc
    return mv
