module JsonSchema
  ( jsonString,
    jsonNumber,
    jsonInteger,
    jsonBool,
    jsonNull,
    jsonArray,
    jsonField,
    (<**>),
    (<=>),
    JsonSchema,
    JsonSchema',
  )
where

import Control.Applicative (Applicative (liftA2))
import JsonParser (Json (..), jsonToInteger)

type JsonSchema v = Json -> Maybe v

jsonString :: JsonSchema String
jsonString (JsonString s) = Just s
jsonString _ = Nothing

jsonNumber :: JsonSchema Float
jsonNumber (JsonNumber n) = Just n
jsonNumber _ = Nothing

jsonInteger :: JsonSchema Int
jsonInteger json = jsonNumber json >>= jsonToInteger

jsonBool :: JsonSchema Bool
jsonBool (JsonBool b) = Just b
jsonBool _ = Nothing

jsonNull :: JsonSchema ()
jsonNull JsonNull = Just ()
jsonNull _ = Nothing

jsonArray :: JsonSchema a -> JsonSchema [a]
jsonArray itemSchema (JsonArray arr) = jsonArray' arr
  where
    jsonArray' [] = Just []
    jsonArray' (x : xs) = liftA2 (:) (itemSchema x) (jsonArray' xs)
jsonArray _ _ = Nothing

-- Тот deleteBy, который сегодня, придумали идиоты.
-- Поэтому я сделал свой
deleteByFirst :: (a -> Bool) -> [a] -> [a]
deleteByFirst _ [] = []
deleteByFirst f (x : xs) = if f x then xs else x : deleteByFirst f xs

jsonField :: String -> (Json -> Maybe a) -> JsonSchema' a b
jsonField key valueSchema (JsonObject fields, f) =
  procResult . f <$> (lookup key fields >>= valueSchema)
  where
    procResult v = (JsonObject $ deleteByFirst ((== key) . fst) fields, v)
jsonField _ _ _ = Nothing

type JsonSchema' a b = (Json, a -> b) -> Maybe (Json, b)

(<**>) ::  ((Json, b) -> Maybe a) -> (a -> Maybe (Json, c)) -> (Json, b) -> Maybe (Json, c)
-- (<**>) :: JsonSchema' a (b -> c) -> JsonSchema' b c -> (Json, a -> b -> c) -> Maybe (Json, c)
(<**>) r l (json, f) = r (json, f) >>= l

-- (<=>) :: ((Json, i) -> Maybe (Json, r)) -> i -> Json -> Maybe r
(<=>) :: ((a1, b) -> Maybe (a, b1)) -> b -> a1 -> Maybe b1
(<=>) schema int json = snd <$> schema (json, int)

infixl 2 <**>

infixl 1 <=>
