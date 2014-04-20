module TimeSeries.Periodogram
( periodogram
, normalizedPeriodogram
, fStat
) where

import TimeSeries.TimeSeries (TimeSeries (..))
import TimeSeries.FFT (dft)
import Data.Complex (magnitude)

periodogram :: TimeSeries Double -> TimeSeries Double
periodogram = map ((** 2) . magnitude) . dft

normalizedPeriodogram :: TimeSeries Double -> TimeSeries Double
normalizedPeriodogram ts = let pgram = periodogram ts
                               total = sum pgram in
                           map (/ total) pgram

-- Given a time series, output Fisher's periodogram test statistic
-- Reject at level alpha iff F-stat exceeds c_alpha
-- c_alpha \approx K(alpha) log(n) / n, where K  refers to Fisher's kappa
fStat :: TimeSeries Double -> Double
fStat = maximum . normalizedPeriodogram
