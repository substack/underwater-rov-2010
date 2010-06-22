-- interpolate temperature data
module ROV.Interpolate (
    Calibration, Temp, readCalibration, interpolate
) where

import Control.Applicative ((<$>))
import Control.Arrow ((***))
import Control.Monad (join)

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V
import Statistics.LinearRegression (linearRegression)

import Data.Word (Word8)
import Data.Ord (comparing)
import Data.List (sortBy)

type RawTemp = Word8
type Temp = Double
type Calibration = (V.Vector Temp, V.Vector Temp)

both = join (***)

readCalibration :: FilePath -> IO Calibration
readCalibration path
    = both V.fromList
    . unzip
    . map (f . map read . words)
    . filter ((/= '#') . head)
    . lines
    <$> readFile path
    where f [x,y] = (y,x)

interpolate :: Calibration -> RawTemp -> Temp
interpolate cal x = m * x' + b where
    (b,m) = uncurry linearRegression nearest
    x' = fromIntegral x
    nearest
        = both V.fromList
        . unzip
        . take 3
        . sortBy (comparing (abs . subtract x' . fst))
        . uncurry zip
        . both V.toList
        $ cal
