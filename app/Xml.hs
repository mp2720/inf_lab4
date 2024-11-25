module Xml (XmlElement (..), XmlChild (..), xmlElement) where

import Text.Printf (printf)

data XmlChild = XmlText String | XmlNode XmlElement

data XmlElement = XmlElement
  { tag :: String,
    attrs :: [(String, String)],
    body :: [XmlChild]
  }

xmlElement :: String -> [(String, String)] -> [XmlChild] -> XmlElement
xmlElement tag' attrs' body' = XmlElement {tag = tag', attrs = attrs', body = body'}

escape :: String -> String
escape ('&' : xs) = "&amp;" ++ escape xs
escape ('"' : xs) = "&quot;" ++ escape xs
escape ('\'' : xs) = "&apos;" ++ escape xs
escape ('<' : xs) = "&lt;" ++ escape xs
escape ('>' : xs) = "&gt;" ++ escape xs
escape (x : xs) = x : escape xs
escape [] = []

instance Show XmlChild where
  show (XmlText s) = s
  show (XmlNode n) = show n

instance Show XmlElement where
  show (XmlElement tag' attrs' body') =
    printf
      "<%s%s>%s</%s>"
      tag'
      (attrsStr attrs')
      (bodyStr body')
      tag'
    where
      attrsStr :: [(String, String)] -> String
      attrsStr =
        concatMap
          ( \(k, v) ->
              printf " %s=\"%s\"" (escape k :: String) (escape v :: String) :: String
          )
      bodyStr [] = ""
      bodyStr (x : xs) = show x ++ concatMap ((' ' :) . show) xs
