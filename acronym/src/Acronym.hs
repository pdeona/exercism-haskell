module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
abbreviate = concatMap capitalizeFirst . words . map filterSep

capitalizeFirst :: String -> String
{- capitalize first letter, return other uppercase 
  letters in word (for camelcase) or just first 
  letter (if word is already acronym, i.e. GNU) -}
capitalizeFirst [] = []
capitalizeFirst (x:xs) = toUpper x : otherCaps xs
  where 
    otherCaps ys
      | allCaps ys = []
      | otherwise  = filter isUpper ys
    allCaps = all isUpper

filterSep :: Char -> Char
-- split words on separators (non alphas)
filterSep x
-- don't split words on apostrophes
  | isAlpha x 
  || x == '\'' = x
  | otherwise  = ' '
