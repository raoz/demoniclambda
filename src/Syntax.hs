module Syntax where

lambda = "λ"
comment = "―"


data Term = Variable String 
          | Abstraction String Term 
          | Application Term Term 
          | Number Integer
          | Plus Term Term
          | Times Term Term
          | Minus Term Term
          | Divide Term Term
          | Boolean Bool deriving (Eq)

instance Show Term
        where
                show (Variable v) = v
                show (Abstraction v t) = lambda <> v <> "." <> show t
                show (Application t1 t2) = "(" <> show t1 <> ") (" <> show t2 <> ")"
                show (Number n) = show n
                show (Boolean True) = "⊤"
                show (Boolean False) = "⊥"
                show (Plus t1 t2) = "(" <> show t1 <> " + " <> show t2 <> ")"
                show (Minus t1 t2) = "(" <> show t1 <> " - " <> show t2 <> ")"
                show (Times t1 t2) = "(" <> show t1 <> " · " <> show t2 <> ")"
                show (Divide t1 t2) = "(" <> show t1 <> " / " <> show t2 <> ")"
