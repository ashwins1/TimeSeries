module TimeSeries.Correlation
( autocov
, acf
, pacf
) where

import TimeSeries.TimeSeries ( TimeSeries (..) )

mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral $ length xs)

autocov :: Int -> TimeSeries Double -> Double
autocov h xs = (1/n) * (sum $ zipWith (*) xcnt (drop h xcnt))
  where n    = fromIntegral $ length xs :: Double
        xbar = mean xs
        xcnt = [x - xbar | x <- xs]

acf :: Int -> TimeSeries Double -> Double
acf h xs = (autocov h xs) / (autocov 0 xs)

pacf :: Int -> TimeSeries Double -> Double
pacf = undefined
