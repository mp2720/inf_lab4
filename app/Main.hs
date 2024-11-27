module Main where

import JsonParser
import Task1
import Task3
import Task5

main :: IO ()
main = do
  inp <- getContents
  print $ jsonStringToXmlScheduleWithAeson inp

  -- case parseJson inp of
  --   Left err -> print err
  --   Right json -> print $ jsonScheduleToCsv json -- jsonToXml "schedule" json
