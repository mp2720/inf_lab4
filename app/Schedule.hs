{-# LANGUAGE DeriveGeneric #-}

module Schedule
  ( Schedule (..),
    Day (..),
    Class (..),
    Auditorium (..),
    scheduleToXml,
    jsonToSchedule,
    scheduleToCsv,
  )
where

import Csv
import Data.Aeson (FromJSON)
import GHC.Generics
import JsonParser
import JsonSchema
import Xml

newtype Schedule = Schedule {days :: [Day]} deriving (Show, Generic)

data Day = Day {ofTheWeek :: String, classes :: [Class]} deriving (Show, Generic)

data Class = Class
  { form :: String,
    startTime :: String,
    endTime :: String,
    subject :: String,
    teacher :: String,
    auditorium :: Auditorium
  }
  deriving (Show, Generic)

data Auditorium = Auditorium {address :: String, number :: Int} deriving (Show, Generic)

-- Aeson

instance FromJSON Schedule

instance FromJSON Day

instance FromJSON Class

instance FromJSON Auditorium

-- XML generation

xfs :: String -> String -> XmlChild
xfs key v = XmlNode $ xmlElement key [XmlText v]

auditoriumToXml :: Auditorium -> XmlElement
auditoriumToXml a =
  xmlElement
    "auditorium"
    [ xfs "address" $ address a,
      xfs "number" $ show $ number a
    ]

classToXml :: Class -> XmlElement
classToXml c =
  xmlElement
    "class"
    [ xfs "form" $ form c,
      xfs "startTime" $ startTime c,
      xfs "endTime" $ endTime c,
      xfs "subject" $ subject c,
      xfs "teacher" $ teacher c,
      XmlNode $ auditoriumToXml $ auditorium c
    ]

dayToXml :: Day -> XmlElement
dayToXml (Day ofTheWeek' classes') =
  xmlElement "day" $ xfs "ofTheWeek" ofTheWeek' : map (XmlNode . classToXml) classes'

scheduleToXml :: Schedule -> XmlElement
scheduleToXml (Schedule days') = xmlElement "schedule" $ map (XmlNode . dayToXml) days'

-- JSON schema

jf :: String -> (Json -> Maybe a) -> JsonSchema' a b
jf = jsonField

jfString :: String -> JsonSchema' String b
jfString key = jf key jsonString

jsonToSchedule :: JsonSchema Schedule
jsonToSchedule = jf "days" (jsonArray jsonToDay) <=> Schedule

jsonToDay :: JsonSchema Day
jsonToDay =
  jfString "ofTheWeek"
    <**> jf "classes" (jsonArray jsonToClass)
    <=> Day

jsonToAuditorium :: JsonSchema Auditorium
jsonToAuditorium = jf "address" jsonString <**> jf "number" jsonInteger <=> Auditorium

jsonToClass :: JsonSchema Class
jsonToClass =
  jfString "form"
    <**> jfString "startTime"
    <**> jfString "endTime"
    <**> jfString "subject"
    <**> jfString "teacher"
    <**> jf "auditorium" jsonToAuditorium
    <=> Class

-- CSV generation

scheduleToCsv :: Schedule -> CsvMatrix
scheduleToCsv (Schedule days') = CsvMatrix $ concatMap dayToCsv days'

dayToCsv :: Day -> [CsvRow]
dayToCsv (Day ofTheWeek' classes') = map (classToCsv ofTheWeek') classes'

classToCsv :: String -> Class -> CsvRow
classToCsv dayOfTheWeek c =
  CsvRow
    [ dayOfTheWeek,
      form c,
      startTime c,
      endTime c,
      subject c,
      teacher c,
      address $ auditorium c,
      show $ number $ auditorium c
    ]
