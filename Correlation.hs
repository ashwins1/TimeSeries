module TimeSeries.Correlation
( autocov
, acf
, pacf
) where

mean :: [Double] -> Double
mean xs = (sum xs) / (length xs)

autocov :: Int -> TimeSeries Double -> Double
autocov h xs = (1/n) * (sum $ zipWith (*) xcnt (drop h xcnt))
  where n    = length ts :: Double
        xbar = mean xs
        xcnt = [x - xbar | x <- xs]

acf :: Int -> TimeSeries Double -> Double
acf h xs = (autocov h xs) / (autocov 0 xs)
