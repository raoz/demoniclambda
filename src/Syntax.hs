module Syntax where

lambda = "λ"
comment = "―"


data Term = Variable String 
          | Abstraction String Term 
          | Application Term Term 
          | Number Integer
          | BinOp String Term Term
          | Boolean Bool deriving (Eq)

instance Show Term
        where
                show (Variable v) = v
                show (Abstraction v t) = lambda <> v <> "." <> show t
                show (Application t1 t2) = "(" <> show t1 <> ") (" <> show t2 <> ")"
                show (Number n) = show n
                show (Boolean True) = "⊤"
                show (Boolean False) = "⊥"
                show (BinOp op t1 t2) = "(" <> show t1 <> " " <> op <> " " <> show t2 <> ")"
