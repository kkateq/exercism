module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Foldable (foldlM)
import Data.Map (Map, adjust, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

-- | Converts character to an Either Char Nucleotide type.
--
-- ==== __Examples__
-- >>> toNucleotide 'A'
-- Right A
-- >>> toNucleotide 'X'
-- Left X
toNucleotide :: Char -> Either Char Nucleotide
toNucleotide 'A' = Right A
toNucleotide 'C' = Right C
toNucleotide 'G' = Right G
toNucleotide 'T' = Right T
toNucleotide x = Left x

-- Updates a value at a specific key by incrementing it by 1.
f :: Map Nucleotide Integer -> Nucleotide -> Map Nucleotide Integer
f acc y = adjust (+ 1) y acc

-- Adjusts a value at a specific key of type Nucleotide by incrementing it by 1.
g :: Map Nucleotide Integer -> Char -> Either Char (Map Nucleotide Integer)
g acc x = fmap (f acc) (toNucleotide x)

-- | Given a string representing a DNA sequence, count how many of each nucleotide is present.
-- If the string contains characters that aren't A, C, G, or T then it is invalid and you should signal an error.
--
-- ==== __Examples__
-- >>> nucleotideCounts "GATTACA"
-- Right (fromList [(A,3),(C,1),(G,1),(T,2)])
-- >>> nucleotideCounts "INVALID"
-- Left 'I'
nucleotideCounts :: [Char] -> Either Char (Map Nucleotide Integer)
nucleotideCounts =
  foldlM g (fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
