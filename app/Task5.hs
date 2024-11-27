module Task5 (jsonScheduleToCsv) where

import Csv
import JsonParser (Json)
import JsonSchema

newtype Schedule = Schedule [Day] deriving (Show)

data Day = Day String [Class] deriving (Show)

data Class = Class
  { type_ :: String,
    startTime :: String,
    endTime :: String,
    subject :: String,
    teacher :: String,
    auditorium :: Auditorium
  }
  deriving (Show)

data Auditorium = Auditorium {address :: String, number :: Int} deriving (Show)

jf :: String -> (Json -> Maybe a) -> JsonSchema' a b
jf = jsonField

jfString :: String -> JsonSchema' String b
jfString key = jf key jsonString

readSchedule :: JsonSchema Schedule
readSchedule = jf "days" (jsonArray readDay) <=> Schedule

readDay :: JsonSchema Day
readDay =
  jfString "ofTheWeek"
    <**> jf "classes" (jsonArray readClass)
    <=> Day

readAuditorium :: JsonSchema Auditorium
readAuditorium = jf "address" jsonString <**> jf "number" jsonInteger <=> Auditorium

readClass :: JsonSchema Class
readClass =
  jfString "type"
    <**> jfString "startTime"
    <**> jfString "endTime"
    <**> jfString "subject"
    <**> jfString "teacher"
    <**> jf "auditorium" readAuditorium
    <=> Class

scheduleToCsv :: Schedule -> CsvMatrix
scheduleToCsv (Schedule days) = CsvMatrix $ concatMap dayToCsv days

dayToCsv :: Day -> [CsvRow]
dayToCsv (Day ofTheWeek classes) = map (classToCsv ofTheWeek) classes

classToCsv :: String -> Class -> CsvRow
classToCsv dayOfTheWeek c =
  CsvRow
    [ dayOfTheWeek,
      type_ c,
      startTime c,
      endTime c,
      subject c,
      teacher c,
      address $ auditorium c,
      show $ number $ auditorium c
    ]

jsonScheduleToCsv :: Json -> Maybe CsvMatrix
jsonScheduleToCsv json = scheduleToCsv <$> readSchedule json
