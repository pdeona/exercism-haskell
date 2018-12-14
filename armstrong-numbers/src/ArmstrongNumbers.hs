module ArmstrongNumbers (armstrong) where

import Data.Char

armstrong :: (Integral a, Show a) => a -> Bool
armstrong = (==) <*> armstrd
  where
    armstrd n = sum $ map (fromIntegral . (^(length strN)) . digitToInt) $ strN
      where strN = show n
