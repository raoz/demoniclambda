{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Debug.Trace

import Syntax
import CPS
import Codegen

import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Text.Parsec.Language (emptyDef)

import System.IO

import System.Console.Haskeline
import Control.Monad.IO.Class

import           TreeSitter.Parser
import           TreeSitter.Tree
import           TreeSitter.Language
import           TreeSitter.DemonicLambda
import           TreeSitter.Node
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Ptr                    ( Ptr(..)
                                                , nullPtr
                                                , plusPtr
                                                )
import           Foreign.Marshal.Alloc          ( malloc
                                                , mallocBytes
                                                )
import           Foreign.Marshal.Array          ( mallocArray
                                                , advancePtr)
import           Foreign.Storable               ( peek
                                                , peekElemOff
                                                , poke
                                                )
import           Foreign.Marshal.Utils          ( new )
import           Control.Monad

import Foreign
import Foreign.C
import GHC.Generics
import Text.Parsec.Error
import Text.Parsec.Pos
import Control.Monad.Except

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as LT

type ParseMonad = ExceptT ParseError IO

substr :: Ptr CChar -> Word32 -> Word32 -> ParseMonad String
substr s nodeStartByte nodeEndByte = liftIO $ peekCStringLen (plusPtr s $ fromIntegral nodeStartByte, fromIntegral $ nodeEndByte - nodeStartByte)


convertTree:: Ptr Node -> Ptr CChar -> ParseMonad Term
convertTree concrete s = do
  Node {..} <- liftIO $ peek concrete
  -- TODO: check if node is missing
  theType <- liftIO $ peekCString nodeType
  let childCount = fromIntegral nodeChildCount
  children <- liftIO $ mallocArray childCount
  tsNode <- liftIO malloc
  liftIO $ poke tsNode nodeTSNode
  liftIO $ ts_node_copy_child_nodes tsNode children
  case theType of
    "program" -> convertTree (advancePtr children 0) s
    "paren_term" -> convertTree (advancePtr children 1) s
    "abstraction" -> do
      term <- convertTree (advancePtr children 3) s
      Node {..} <- liftIO $ peekElemOff children 1
      bound <- substr s nodeStartByte nodeEndByte
      return $ Abstraction bound term
    "application" -> do
      left <- convertTree (advancePtr children 0) s
      right <- convertTree (advancePtr children 1) s
      return $ Application left right
    "variable" -> do
      name <- substr s nodeStartByte nodeEndByte
      return $ Variable name
    "bin_op" -> do
      left <- convertTree (advancePtr children 0) s
      right <- convertTree (advancePtr children 2) s
      Node {..} <- liftIO $ peekElemOff children 1
      operator <- substr s nodeStartByte nodeEndByte
      return $ BinOp operator left right
    "number" -> do
      value <- substr s nodeStartByte nodeEndByte
      return . Number $ read value
    "boolean" -> do
      value <- substr s nodeStartByte nodeEndByte
      case value of
        "⊤" -> return $ Boolean True
        "⊥" -> return $ Boolean False
    "ERROR" -> do
      let TSPoint {..} = nodeStartPoint
      element <- substr s nodeStartByte nodeEndByte
      throwError $ newErrorMessage (UnExpect element) (newPos "<stdin>" (fromIntegral pointRow) (fromIntegral pointColumn))


parseTerm :: String -> IO(Either ParseError Term)
parseTerm s = do
  parser <- ts_parser_new
  ts_parser_set_language parser tree_sitter_demoniclambda

  (str, len) <- newCStringLen s
  tree       <- ts_parser_parse_string parser nullPtr str len
  n          <- malloc
  ts_tree_root_node_p tree n

  runExceptT $ convertTree n str




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

                eval' c (BinOp op (Number a) (Number b)) = case op of
                        "+" -> Number (a + b)
                        "-" -> Number (a - b)
                        "/" -> Number (a `div` b)
                        "·" -> Number (a * b)
                eval' c (BinOp op (Boolean a) (Boolean b)) = case op of
                        "∧" -> Boolean (a && b)
                        "∨" -> Boolean (a || b)
                eval' c (BinOp op a b)
                        | (BinOp op a b) == reduced = BinOp op a b
                        | otherwise = eval c reduced
                        where 
                                reduced = BinOp op (eval c a) (eval c b)

                eval' c x = x





main :: IO ()
main = runInputT defaultSettings loop
        where
                loop = do
                        ms <- getInputLine "→ "
                        case ms of
                                Nothing -> return ()
                                Just s -> do
                                        p <- liftIO (parseTerm s)
                                        case p of
                                                Left err -> outputStrLn . show $ err
                                                Right ast -> outputStrLn . show $ eval M.empty ast
                                                --Right ast -> do
                                                --        outputStrLn . LT.unpack . pShow $ transformCPS ast
                                                --        liftIO . codegen $ transformCPS ast
                                                --        return ()
                                        loop
