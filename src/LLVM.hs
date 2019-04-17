module LLVM where

import Prelude hiding (and, or, pred)

import Control.Monad.State (gets)
import qualified Data.Map.Lazy as Map
import Data.Word
import Data.Char (ord)

import LLVM.AST hiding (args, dests)
import LLVM.AST.Type as AST
import LLVM.AST.Typed
import LLVM.AST.ParameterAttribute
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.FloatingPointPredicate as FP

import LLVM.AST.Global
import LLVM.AST.Linkage

import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module
import qualified LLVM.AST.Instruction as I

import Control.Applicative
import Control.Monad

fastCall :: MonadIRBuilder m => Operand -> [(Operand, [ParameterAttribute])] -> m Operand 
fastCall fun args = do
  let instr = Call {
    AST.tailCallKind = Just I.MustTail
  , AST.callingConvention = CC.Fast
  , AST.returnAttributes = []
  , AST.function = Right fun
  , AST.arguments = args
  , AST.functionAttributes = []
  , AST.metadata = []
  }
  case typeOf fun of
      FunctionType r _ _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef AST.void)))
        _        -> emitInstr r instr
      PointerType (FunctionType r _ _) _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef AST.void)))
        _        -> emitInstr r instr
      _ -> error "Cannot call non-function (Malformed AST)."

fastCallNoTail :: MonadIRBuilder m => Operand -> [(Operand, [ParameterAttribute])] -> m Operand 
fastCallNoTail fun args = do
  let instr = Call {
    AST.tailCallKind = Nothing
  , AST.callingConvention = CC.Fast
  , AST.returnAttributes = []
  , AST.function = Right fun
  , AST.arguments = args
  , AST.functionAttributes = []
  , AST.metadata = []
  }
  case typeOf fun of
      FunctionType r _ _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef AST.void)))
        _        -> emitInstr r instr
      PointerType (FunctionType r _ _) _ -> case r of
        VoidType -> emitInstrVoid instr >> (pure (ConstantOperand (C.Undef AST.void)))
        _        -> emitInstr r instr
      _ -> error "Cannot call non-function (Malformed AST)."

fastFun
  :: MonadModuleBuilder m
  => Name  -- ^ Function name
  -> [(Type, ParameterName)]  -- ^ Parameter types and name suggestions
  -> Type  -- ^ Return type
  -> ([Operand] -> IRBuilderT m ())  -- ^ Function body builder
  -> m Operand
fastFun label argtys retty body = do
  let tys = fst <$> argtys
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference tys paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith (\ty nm -> Parameter ty nm []) tys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      , LLVM.AST.Global.callingConvention = CC.Fast
      }
    funty = ptr $ FunctionType retty (fst <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ C.GlobalReference funty label
