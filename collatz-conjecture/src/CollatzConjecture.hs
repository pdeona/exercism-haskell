module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz i
  | i <= 0    = Nothing
  | otherwise = Just (fromIntegral . length $ steps i)
  where
    steps n
      | n == 1 = []
      | even n = n : steps (n `div` 2)
      | odd n  = n : steps (3 * n + 1)
      | otherwise = []
