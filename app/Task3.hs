module Task3 (jsonToXml) where

import JsonParser
import Xml

jsonToXml :: String -> Json -> XmlElement
jsonToXml rootElementTagName json = xmlElement rootElementTagName [] $ jsonToXml' json

jsonToXml' :: Json -> [XmlChild]
jsonToXml' (JsonNumber n) = [XmlText showNumber]
  where
    showNumber = case n - floored of
      0 -> show (floor floored :: Int)
      _ -> show n
      where
        floored :: Float
        floored = fromIntegral (floor n :: Int)
jsonToXml' (JsonString s) = [XmlText s]
jsonToXml' (JsonBool b) = [XmlText $ show b]
jsonToXml' JsonNull = [XmlText "null"]
jsonToXml' (JsonArray arr) = arrToXml "item" arr
jsonToXml' (JsonObject fields) = concatMap convertField fields
  where
    convertField (k, JsonArray arr) = arrToXml k arr
    convertField (k, v) = [XmlNode $ xmlElement k [] $ jsonToXml' v]

arrToXml :: String -> [Json] -> [XmlChild]
arrToXml itemTag = map (XmlNode . xmlElement itemTag [] . jsonToXml')
