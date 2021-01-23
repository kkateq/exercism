module DNA (toRNA) where

-- | Transcribes ones of the DNA nucleotide elements (G,C,T or A) into
-- appropriate RNA nucleotide element (C,G,A or U).
-- For an invalid element returns this element without any changes.
--
-- ==== __Examples__
-- >>> toRNA' 'A'
-- 'U'
-- >>> toRNA' 'X'
-- 'X'
toRNA' :: Char -> Either Char Char
toRNA' 'G' = Right 'C'
toRNA' 'C' = Right 'G'
toRNA' 'T' = Right 'A'
toRNA' 'A' = Right 'U'
toRNA' x = Left x

-- | Given a DNA strand, return its RNA complement (per RNA transcription).
--
-- ==== __Examples__
-- >>> toRNA "ACGTGGTCTTAA"
-- Right "UGCACCAGAAUU"
-- >>> toRNA ""
-- Right ""
-- >>> toRNA "ACGTXXXCTTAA"
-- Left 'X'
toRNA :: String -> Either Char String
toRNA = mapM toRNA'
