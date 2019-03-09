module Main where
import Debug.Trace

import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (emptyDef)

import System.IO

import System.Console.Haskeline


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

lambda = "λ"
comment = "―"


lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
        where
                style = emptyDef {
                        Tok.reservedOpNames = ["+"],
                        Tok.commentLine = comment,
                        Tok.reservedNames = [lambda],
                        Tok.identStart   = letter <|> char '_',
                        Tok.identLetter  = alphaNum <|> char '_'
                }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

symbol :: String -> Parser ()
symbol s = Tok.symbol lexer s >> return ()

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

dot :: Parser ()
dot = Tok.dot lexer >> return ()

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer >> return ()









abstraction :: Parser Term
abstraction = parens $ do
        symbol lambda
        name <- identifier
        dot
        t <- term
        return $ Abstraction name t


application :: Parser Term
application = parens $ do
        t1 <- term
        t2 <- term
        return $ Application t1 t2

number :: Parser Term
number = do
        n <- integer
        return $ Number n

binary s f assoc = Ex.Infix (reservedOp s >> return f) assoc

table = [[binary "·" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

expr :: Parser Term
expr = Ex.buildExpressionParser table term

bTrue :: Parser Term
bTrue = symbol "⊤" >> return (Boolean True)
bFalse :: Parser Term
bFalse = symbol "⊥" >> return (Boolean False)

boolean :: Parser Term
boolean = try bTrue <|> try bFalse

variable = identifier >>= return . Variable


term :: Parser Term
term = try (parens expr) <|> try abstraction <|> try application <|> try variable <|> try number <|> try boolean

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r


parseTerm :: String -> Either ParseError Term
parseTerm s = parse (contents term) "<stdin>" s

type Context = M.Map String Term

eval :: Context -> Term -> Term
eval c t = trace ("C: " <> show c <> " T: " <> show t) (eval' c t)
        where
                eval' :: Context -> Term -> Term
                eval' c (Variable x)
                        | x `M.member` c = eval (M.delete x c) (c M.! x)
                        | otherwise    = Variable x 
                eval' c (Abstraction v m) 
                        | reduced == m = Abstraction v m
                        | otherwise = Abstraction v reduced
                        where
                                reduced = eval c m
                eval' c (Application (Abstraction v x) m) = eval (M.insert v m c) x
                eval' c (Application t m) 
                        | reducedAbs == t = Application t m
                        | otherwise = eval c (Application reducedAbs m)
                        where
                                reducedAbs = eval c t

                eval' c (Plus (Number a) (Number b)) = Number (a + b)
                eval' c (Plus a b)
                        | (Plus a b) == reduced = Plus a b
                        | otherwise = eval c reduced
                        where 
                                reduced = Plus (eval c a) (eval c b)
                eval' c (Times (Number a) (Number b)) = Number (a * b)
                eval' c (Times a b)
                        | (Times a b) == reduced = Times a b
                        | otherwise = eval c reduced
                        where 
                                reduced = Times (eval c a) (eval c b)
                eval' c (Minus (Number a) (Number b)) = Number (a - b)
                eval' c (Minus a b)
                        | (Minus a b) == reduced = Minus a b
                        | otherwise = eval c reduced
                        where 
                                reduced = Minus (eval c a) (eval c b)
                eval' c (Divide (Number a) (Number b)) = Number (a `div` b)
                eval' c (Divide a b)
                        | (Divide a b) == reduced = Divide a b
                        | otherwise = eval c reduced
                        where 
                                reduced = Divide (eval c a) (eval c b)
                eval' c x = x


main :: IO ()
main = runInputT defaultSettings loop
        where
                loop = do
                        ms <- getInputLine "→ "
                        case ms of
                                Nothing -> return ()
                                Just s -> do
                                        let p = parseTerm s
                                        case p of
                                                Left err -> outputStrLn . show $ err
                                                Right ast -> outputStrLn . show $ eval M.empty ast
                                        loop

