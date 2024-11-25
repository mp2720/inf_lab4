module Main where

import JsonParser
import Task3

main :: IO ()
main = do
  inp <- getContents
  case parseJsonFromString inp of
    Left err -> print err
    Right json -> print $ jsonToXml "schedule" json
