module Task1 where

import Data.Aeson (decode)
import Data.ByteString.Lazy.UTF8 (fromString)
import Schedule
import Xml

jsonStringToXmlScheduleWithAeson :: String -> Maybe XmlElement
jsonStringToXmlScheduleWithAeson s = scheduleToXml <$> decode (fromString s)
