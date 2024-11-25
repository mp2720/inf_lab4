module CombParser where

data Terms term = Terms {rems :: [term], lineno :: Int} deriving (Show)

data ParserResult term v = Ok v (Terms term) | Error Int deriving (Show)

type Parser term v = Terms term -> ParserResult term v

type Interpr a b = a -> b

(<**>) :: Parser t v1 -> Parser t v2 -> Parser t (v1, v2)
(<**>) a b ts = pa (a ts)
  where
    pa (Ok ra ts') = case b ts' of
      Ok rb ts'' -> Ok (ra, rb) ts''
      Error e -> Error e
    pa (Error e) = Error e

(<|>) :: Parser t v -> Parser t v -> Parser t v
(<|>) a b ts = pa (a ts)
  where
    pa (Error _) = b ts
    pa ra = ra

(<=>) :: Parser t v -> Interpr v i -> Parser t i
(<=>) p int ts = case p ts of
  Ok r ts' -> Ok (int r) ts'
  Error e -> Error e

(<=>>) :: Parser t (v1, v2) -> Interpr v1 (v2 -> i) -> Parser t i
(<=>>) p int = p <=> uncurry int

termIf :: (t -> Bool) -> Parser t t
termIf _ (Terms {rems = [], lineno = l}) = Error l
termIf f (Terms {rems = (x : xs), lineno = l}) =
  if f x
    then
      Ok x (Terms {rems = xs, lineno = l})
    else Error l

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
end Terms {rems = [], lineno = l} = Ok () (Terms {rems = [], lineno = l})
end Terms {lineno = l} = Error l

enclosed :: (Eq t) => t -> t -> Parser t v -> Parser t v
enclosed l r p =
  termEq l <**> p <**> termEq r <=> \(_, (res, _)) -> res

infixr 7 <**>

infixr 6 <|>

infixr 5 <=>

infixr 5 <=>>
