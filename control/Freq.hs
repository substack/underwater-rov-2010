module Main where
import Sound.Alsa
import DSP.Estimation.Frequency.Pisarenko (pisarenko)
import Numeric.Transform.Fourier.FFT (ifft)

import Foreign (Ptr, Storable, mallocArray, peekArray)
import Control.Monad (forever)
import Data.Array (listArray)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar

type MicState = (Double,Double) -- frequency, volume

main :: IO ()
main = do
    micV <- micThread 44100 4000
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
        
        let -- phase angle is in radians
            volume = (sum $ map abs rawSound) / fromIntegral n
            phaseAngle = pisarenko (listArray (0,n-1) rawSound)
            --phaseAngle = ifft (listArray (0,n-1) rawSound)
            hz = phaseAngle / (2 * pi) * fromIntegral sampleRate
        putMVar mv $ if isNaN hz then (0,0) else (hz,volume)
    return mv
