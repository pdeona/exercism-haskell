module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = foldl (+) 0 (mults factors)
  where
    mults :: [Integer] -> [Integer]
    mults       = (nub . concat . map factorize)
    factorize 0 = [0]
    factorize f = filter ((== 0) . (`mod` f)) [f..(limit - 1)]
