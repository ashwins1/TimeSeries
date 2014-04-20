module TimeSeries.FFT
( dft
) where

import TimeSeries.TimeSeries (TimeSeries (..))
import Data.Complex (Complex (..), cis)

splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd xs = splitEvenOdd' xs True ([], [])
  where splitEvenOdd' [] _ (evens, odds) = (reverse evens, reverse odds)
        splitEvenOdd' (x:xs) isEvenIdx (evens, odds)
          | isEvenIdx = splitEvenOdd' xs False (x:evens, odds)
          | otherwise = splitEvenOdd' xs True (evens, x:odds)

-- FIXME : currently only works with time series of length 2^n for some n
dft :: TimeSeries Double -> TimeSeries (Complex Double)
dft []    = []
dft [val] = [val :+ 0]
dft ts = let n = fromIntegral $ length ts :: Double
             (tsEvens, tsOdds) = splitEvenOdd ts
             dftCombined = do (k, f_even, f_odd) <- zip3 ([0 .. (n / 2) - 1] :: [Double]) (dft tsEvens) (dft tsOdds)
                              let t = f_even
                              let f_even' = t + cis (-2 * pi * (k/n)) * f_odd
                              let f_odd'  = t - cis (-2 * pi * (k/n)) * f_odd
                              return (f_even', f_odd')
             in ((map fst dftCombined) ++ (map snd dftCombined))
