module JsonParser (Json (..), parseJson) where

import CombParser
import Data.Char (chr, ord)

data Json
  = JsonNumber Float
  | JsonString String
  | JsonBool Bool
  | JsonNull
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Show)

type JsonParser = Parser Char Json

term :: String -> Parser Char String
term = termEqMany

decDigit :: Parser Char Char
decDigit = termRange '0' '9'

jsonNumber :: JsonParser
jsonNumber =
  optional minus ""
    <**> intPart
    <**> optional fracPart ""
    <**> optional ePart ""
    <=> \(s, (i, (f, e))) -> JsonNumber $ read (s ++ i ++ f ++ e)
  where
    minus = term "-"
    sign = minus <|> term "+"
    digits = decDigit <**> (digits <|> empty "") <=>> (:)

    intPart = term "0" <|> noLzDigits
    noLzDigits = termRange '1' '9' <**> optional digits "" <=>> (:)

    fracPart = term "." <**> digits <=>> (++)

    ePart =
      (term "e" <|> term "E")
        <**> optional sign ""
        <**> digits
        <=> \(e, (s, d)) -> e ++ s ++ d

stringLiteral :: Parser Char String
stringLiteral =
  term "\"" <**> content <**> term "\"" <=> \(_, (s, _)) -> s
  where
    content = (char <**> content <=>> (:)) <|> empty ""
    char = esc <|> regular
    regular = termIf (\c -> c /= '"' && c /= '\n')

    termRangeToInt offset from to = termRange from to <=> \c -> offset + ord c - ord from
    hexDigit =
      termRangeToInt 0 '0' '9'
        <|> termRangeToInt 10 'a' 'f'
        <|> termRangeToInt 10 'A' 'F'

    hexDigit4 =
      hexDigit
        <**> hexDigit
        <**> hexDigit
        <**> hexDigit
        <=> \(d1, (d2, (d3, d4))) -> d1 * 4096 + d2 * 256 + d3 * 16 + d4

    hexEsc = termEq 'u' <**> hexDigit4 <=> chr . snd

    simpleEsc c1 c2 = termEq c1 <=> const c2

    esc =
      termEq '\\'
        <**> ( hexEsc
                 <|> simpleEsc '"' '"'
                 <|> simpleEsc '\\' '\\'
                 <|> simpleEsc '/' '/'
                 <|> simpleEsc 'b' '\b'
                 <|> simpleEsc 'f' '\f'
                 <|> simpleEsc 'n' '\n'
                 <|> simpleEsc 'r' '\r'
                 <|> simpleEsc 't' '\t'
             )
        <=> snd

jsonString :: JsonParser
jsonString = stringLiteral <=> JsonString

jsonBool :: JsonParser
jsonBool =
  (term "true" <=> const (JsonBool True))
    <|> (term "false" <=> const (JsonBool False))

jsonNull :: JsonParser
jsonNull = term "null" <=> const JsonNull

jsonWhitespace :: Parser Char ()
jsonWhitespace =
  (spaceChar <**> jsonWhitespace <=> const ()) <|> empty ()
  where
    spaceChar = termEq ' ' <|> termEq '\r' <|> termEq '\t' <|> termEq '\n' <=> const ()

commaSeparated :: Parser Char v -> Parser Char [v]
commaSeparated item =
  (item <**> items <=>> (:)) <|> (jsonWhitespace <=> const [])
  where
    items = (termEq ',' <**> item <**> items <=> \(_, (i, is)) -> i : is) <|> empty []

jsonArray :: JsonParser
jsonArray = enclosed '[' ']' $ commaSeparated value <=> JsonArray

jsonObject :: JsonParser
jsonObject = enclosed '{' '}' $ commaSeparated field <=> JsonObject
  where
    field =
      jsonWhitespace
        <**> stringLiteral
        <**> jsonWhitespace
        <**> termEq ':'
        <**> value
        <=> \(_, (k, (_, (_, v)))) -> (k, v)

value :: JsonParser
value = jsonWhitespace <**> content <**> jsonWhitespace <=> \(_, (v, _)) -> v
  where
    content =
      jsonNumber
        <|> jsonString
        <|> jsonBool
        <|> jsonNull
        <|> jsonObject
        <|> jsonArray

json :: JsonParser
json = value <**> end <=> fst

parseJson :: String -> Either String Json
parseJson s = case json s of
  Error -> Left "JSON syntax error"
  Ok v _ -> Right v
