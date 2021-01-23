module Pangram (isPangram) where

import Data.Char (toLower)
import Prelude hiding (filter)

alphabet :: [Char]
alphabet = ['a' .. 'z']

isPangram :: String -> Bool
isPangram text = all (`elem` map toLower text) alphabet
