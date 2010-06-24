module ROV.Mic (
    Assoc(..), Pair, Frequency, Amplitude, listen
) where

import Sound.Alsa
import Numeric.Transform.Fourier.FFT (fft)
import qualified Data.Array as Ax

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Monad (forever)
import Data.Complex (Complex(..),magnitude)

type Frequency = Double
type Amplitude = Double
type Pair = (Frequency,Amplitude)
type Assoc = [Pair]
type Device = String

-- takes device name, such as "plughw:0,0", sample rate, number of samples, and
-- provides an association list of frequencies and amplitudes
listen :: Device -> Int -> Int -> IO Assoc
listen dev sampleRate samples = do
    let
        source = alsaSoundSource dev soundFormat
        soundFormat = SoundFmt sampleRate :: SoundFmt Double
    buf <- mallocArray samples
    
    withSoundSource source $ \handle -> do
        n <- soundSourceRead source handle buf samples
        rawSound <- peekArray n buf
        
        let
            amps = map magnitude $ Ax.elems $ fft
                $ Ax.listArray (0,n-1) $ map (:+ 0)
                $ [ s * 0.5 * (1 - cos t)
                    | (s,t) <- zip rawSound [ 0, dt .. 2 * pi ] ]
            dt = 2 * pi / (fromIntegral $ length rawSound)
            sampleStep = fromIntegral sampleRate / fromIntegral n
            freqs = [ 0, sampleStep .. fromIntegral sampleRate / 2 ]
            freqAssoc = [ (f,a) | (f,a) <-  zip freqs amps, f > 500 && f < 5500 ]
        
        return freqAssoc
