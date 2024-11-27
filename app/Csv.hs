module Csv (escape, CsvRow(..), CsvMatrix(..)) where

import Data.List (intercalate)

escape :: String -> String
escape s = if ',' `elem` s then "\"" ++ escapeQuotes s ++ "\"" else s
  where
    escapeQuotes ('"' : xs) = '"' : '"' : escapeQuotes xs
    escapeQuotes (x : xs) = x : escapeQuotes xs
    escapeQuotes [] = ""

newtype CsvRow = CsvRow [String]

newtype CsvMatrix = CsvMatrix [CsvRow]

instance Show CsvRow where
  show (CsvRow cols) = intercalate "," $ map escape cols

instance Show CsvMatrix where
  show (CsvMatrix rows) = intercalate "\n" $ map show rows
