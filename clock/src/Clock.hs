module Clock (addDelta, fromHourMin, toString) where

import Text.Printf

data Clock = Clock Int Int

instance Eq Clock where
  (==) (Clock x y) (Clock i j) = x == i && y == j

instance Show Clock where
  show (Clock h m) = showTime h ++ ":" ++ showTime m
    where showTime = printf "%02d"

fromHourMin :: Int -> Int -> Clock
fromHourMin h m 
  | h >= 24   = fromHourMin (h - 24) m
  | h < 0     = fromHourMin (h + 24) m
  | m >= 60   = fromHourMin (h + 1) (m - 60)
  | m < 0     = fromHourMin (h - 1) (m + 60)
  | otherwise = Clock h m

addClocks :: Clock -> Clock -> Clock
addClocks (Clock h m) (Clock i j) = fromHourMin (h + i) (m + j)

toString :: Clock -> String
toString c = show c

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minutes c = addClocks (fromHourMin hour minutes) c
