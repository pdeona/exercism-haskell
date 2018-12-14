module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x
  | x <= 0         = Nothing
  | x == aliquot x = Just Perfect
  | x > aliquot x  = Just Deficient
  | x < aliquot x  = Just Abundant
  | otherwise      = Nothing
    where aliquot = foldl (+) 0 . factors

factors :: Int -> [Int]
-- create the list of factors for integer x
factors x 
  | x <= 1    = []
  | otherwise = [y | y <- [1..x `div` 2], y /! x]
  where
    -- infix factorOf operator
    (/!) x' y = y `mod` x' == 0
