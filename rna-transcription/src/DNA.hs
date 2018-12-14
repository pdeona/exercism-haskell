module DNA (toRNA) where

import Data.Maybe
import Data.Either

data Nucleotide
  = Adenine
  | Cytosine
  | Guanine
  | Thymine
  | Uracil
  deriving (Enum)

toChar :: Int -> Maybe Char
fromChar :: Char -> Maybe Int
toChar c = case c of
    0 -> Just 'A'
    1 -> Just 'C'
    2 -> Just 'G'
    3 -> Just 'T'
    4 -> Just 'U'
    _ -> Nothing
fromChar c = case c of
    'A' -> Just 0
    'C' -> Just 1
    'G' -> Just 2
    'T' -> Just 3
    'U' -> Just 4
    _   -> Nothing

toRNA :: String -> Either Char String
toRNA xs = case all isRight $ rna of
  True -> Right $ map (fromRight 'a') rna
  False -> Left $ (fromLeft 'x') . head $ takeWhile isLeft $ dropWhile isRight $ rna
  where rna = map convert xs

convert :: Char -> Either Char Char
convert c = case fromChar c of
  Nothing -> Left c
  Just ch  -> case (transcribe . toEnum) ch of
    Nothing -> Left c
    Just r  -> Right $ (fromJust . toChar . fromEnum) r
transcribe :: Nucleotide -> Maybe Nucleotide
transcribe Adenine  = Just Uracil
transcribe Cytosine = Just Guanine
transcribe Guanine  = Just Cytosine
transcribe Thymine  = Just Adenine
transcribe _        = Nothing
