{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Debug.Trace

import AST
import CPS
import Codegen

import qualified Data.Map as M

import System.IO
import System.Environment

import System.Console.Haskeline
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

import GHC.Generics
import Control.Monad
import Control.Monad.Except

import Text.Pretty.Simple (pShow)
import qualified Data.Text.Lazy as LT

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
main = do
        args <- getArgs
        case length args of
                1 -> do
                        code <- liftIO . readFile $ head args
                        p <- liftIO (parseTerm code)
                        case p of
                                Left err -> putStrLn . show $ err
                                Right ast -> do
                                        putStrLn . show $ ast
                                --        putStrLn . show $ eval M.empty ast
                                --Right ast -> do
                                        putStrLn . LT.unpack . pShow $ transformCPS ast
                                        liftIO . codegen $ transformCPS ast
                                        return ()


                otherwise -> runInputT defaultSettings loop
                where
                        loop = do
                                ms <- getInputLine "→ "
                                case ms of
                                        Nothing -> return ()
                                        Just s -> do
                                                p <- liftIO (parseTerm s)
                                                case p of
                                                        Left err -> outputStrLn . show $ err
                                                        Right ast ->
                                                                outputStrLn . show $ eval M.empty ast
                                                        --Right ast -> do
                                                        --        outputStrLn . LT.unpack . pShow $ transformCPS ast
                                                        --        liftIO . codegen $ transformCPS ast
                                                        --        return ()
                                                loop
