module Main where

import Csv
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import JsonParser
import JsonToXml (jsonToXml)
import Schedule
import Xml

justOrFail :: String -> Maybe a -> a
justOrFail _ (Just a) = a
justOrFail e Nothing = error e

rightOrFail :: Either [Char] b -> b
rightOrFail (Right a) = a
rightOrFail (Left b) = error b

jsonStringToScheduleOrFail :: String -> Schedule
jsonStringToScheduleOrFail s =
  justOrFail "invalid json schema" $
    jsonToSchedule $
      rightOrFail $
        parseJson s

task1ByteString :: ByteString -> XmlElement
task1ByteString bs = scheduleToXml $ justOrFail "invalid json syntax/schema" $ decode bs

task1 :: String -> XmlElement
task1 s = task1ByteString $ fromString s

task3 :: String -> XmlElement
task3 = scheduleToXml . jsonStringToScheduleOrFail

task3AlmostUniversal :: String -> XmlElement
task3AlmostUniversal s = jsonToXml "root" (rightOrFail $ parseJson s)

repn :: Int -> (Int -> a) -> [a]
repn 0 _ = []
repn n f = f n : repn (n - 1) f

task5 :: String -> CsvMatrix
task5 = scheduleToCsv . jsonStringToScheduleOrFail

main :: IO ()
main = do
  inp <- getContents
  print $ task5 inp
