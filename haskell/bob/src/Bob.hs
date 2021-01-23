{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import Data.Char (isUpper, toLower)
import Data.Text (Text, all, filter, last, null, strip)
import Prelude hiding (all, filter, last, null)

alphabet :: String
alphabet = ['a' .. 'z']

responseFor :: Text -> Text
responseFor xs
  | null preppedText = "Fine. Be that way!"
  | isUpperCase && isQuestion = "Calm down, I know what I'm doing!"
  | isUpperCase = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    preppedText = strip xs
    chars = filter (\c -> toLower c `elem` alphabet) preppedText
    isUpperCase = all isUpper chars && not (null chars)
    isQuestion = last preppedText == '?'
