module CombParser where

type Terms term = [term]

data ParserResult term v = Ok v (Terms term) | Error deriving (Show)

type Parser term v = Terms term -> ParserResult term v

type Interpr a b = a -> b

(<**>) :: Parser t v1 -> Parser t v2 -> Parser t (v1, v2)
(<**>) a b ts = pa (a ts)
  where
    pa (Ok ra ts') = case b ts' of
      Ok rb ts'' -> Ok (ra, rb) ts''
      Error -> Error
    pa Error = Error

(<|>) :: Parser t v -> Parser t v -> Parser t v
(<|>) a b ts = pa (a ts)
  where
    pa Error = b ts
    pa ra = ra

(<=>) :: Parser t v -> Interpr v i -> Parser t i
(<=>) p int ts = case p ts of
  Ok r ts' -> Ok (int r) ts'
  Error -> Error

(<=>>) :: Parser t (v1, v2) -> Interpr v1 (v2 -> i) -> Parser t i
(<=>>) p int = p <=> uncurry int

termIf :: (t -> Bool) -> Parser t t
termIf _ [] = Error
termIf f (x : xs) =
  if f x
    then
      Ok x xs
    else Error

termEq :: (Eq t) => t -> Parser t t
termEq c = termIf (c ==)

termEqMany :: (Eq t) => [t] -> Parser t [t]
termEqMany [] = Ok []
termEqMany (t : ts) =
  termEq t <**> termEqMany ts <=>> (:)

termRange :: (Ord t) => t -> t -> Parser t t
termRange from to = termIf (\c -> from <= c && c <= to)

empty :: v -> Parser t v
empty = Ok

optional :: Parser t v -> v -> Parser t v
optional p e = p <|> empty e

end :: Parser t ()
end [] = Ok () []
end _ = Error

enclosed :: (Eq t) => t -> t -> Parser t v -> Parser t v
enclosed l r p =
  termEq l <**> p <**> termEq r <=> \(_, (res, _)) -> res

infixr 7 <**>

infixr 6 <|>

infixr 5 <=>

infixr 5 <=>>
