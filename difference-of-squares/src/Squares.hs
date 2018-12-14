module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference = (-) <$> squareOfSum <*> sumOfSquares

squareOfSum :: Integral a => a -> a
squareOfSum = sq . sum . range

sumOfSquares :: Integral a => a -> a
sumOfSquares = sum . map sq . range

sq :: Integral a => a -> a
sq = (^ 2)
range :: Integral a => a -> [a]
range n = [1..n]
