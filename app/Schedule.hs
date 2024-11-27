{-# LANGUAGE DeriveGeneric #-}

module Schedule (Schedule (..), Day (..), Class (..), Auditorium (..), scheduleToXml) where

import Data.Aeson (FromJSON)
import GHC.Generics
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

instance FromJSON Schedule

instance FromJSON Day

instance FromJSON Class

instance FromJSON Auditorium

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
      xfs "startTime" $ show $ startTime c,
      xfs "endTime" $ show $ endTime c,
      xfs "subject" $ show $ subject c,
      xfs "teacher" $ show $ teacher c,
      XmlNode $ auditoriumToXml $ auditorium c
    ]

dayToXml :: Day -> XmlElement
dayToXml (Day ofTheWeek' classes') =
  xmlElement "day" $ xfs "ofTheWeek" ofTheWeek' : map (XmlNode . classToXml) classes'

scheduleToXml :: Schedule -> XmlElement
scheduleToXml (Schedule days) = xmlElement "schedule" $ map (XmlNode . dayToXml) days
