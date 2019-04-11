{-# LANGUAGE ViewPatterns #-}
module Codegen where

import CPS
import Control.Monad.State

import Debug.Trace

import Control.Monad.Except

import Data.ByteString.Short
import qualified Data.ByteString.Char8 as BS


import Data.Word
import Data.String
import Data.List
import Data.Function
import Data.Char (isAscii)

import qualified Data.Map as M
import Text.Pretty.Simple (pShow)

import LLVM.Analysis


import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding (encodeUtf8)

-- Pure
import LLVM.AST hiding (function)
import LLVM.AST.Typed
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction as I
import LLVM.IRBuilder.Constant

--Nonpure
import LLVM.Analysis
import LLVM.Context
import qualified LLVM.Module as LVM

import qualified Data.Text.Punycode as Punycode


encodename :: String -> ShortByteString
encodename str 
        | all isAscii str =  fromString str
        | otherwise = toShort . Punycode.encode . T.pack $ str

codename :: String -> Name
codename = Name . encodename


-----------
-- TYPES --
-----------
voidptr :: AST.Type
voidptr = AST.ptr AST.i8

num :: AST.Type
num = AST.i32

consti32 :: Integer -> Operand
consti32 n = ConstantOperand $ C.Int 32 n

nullptr ::Operand
nullptr = ConstantOperand $ C.IntToPtr (C.Int 64 0) voidptr

demonicfun = AST.FunctionType voidptr [voidptr, voidptr, voidptr] False

closure :: AST.Type
closure = AST.StructureType False [
        ptr demonicfun,
        voidptr ]

context :: [String] -> Type
context frees = AST.ArrayType (fromIntegral $ Data.List.length frees) voidptr


toSig :: [String] -> [(Type, ParameterName)]
toSig = zip (repeat voidptr) . map (ParameterName . encodename)


demonicfunref :: MonadModuleBuilder m => Name -> m Operand
demonicfunref name = pure $ ConstantOperand $ C.GlobalReference (ptr demonicfun) name

primref :: MonadModuleBuilder m => String -> m Operand
primref name = pure . ConstantOperand $ C.GlobalReference (ptr . maybe (error $ "Undefined primop " <> name) id . lookup trueName $ primExternalType) trueName
        where
                trueName = codename name



cgenc :: (MonadIRBuilder m, MonadModuleBuilder m) => (String -> Operand) -> [String] -> Cexp -> m Operand
cgenc localArgs frees (CApplication f args) = do
        --traceM ("application " <> show f <> " " <> show args )
        left <- cgena localArgs frees f
        leftCast <- bitcast left $ ptr closure
        --traceM ("left " <> show left <> " leftCast " <> (show $ typeOf left))
        --traceShowM $ typeOf left
        --traceM "\n"
        --traceM "*leftCast"
        --traceShowM $ getElementType (typeOf leftCast)
        --traceM "\n"
        funpPos <- gep leftCast [consti32 0, consti32 0]
        ctxPos  <- gep leftCast [consti32 0, consti32 1]
        funp <- load funpPos 0
        ctx  <- load ctxPos 0
        argOps <- mapM (cgena localArgs frees) args
        argVOps <- mapM (\arg -> bitcast arg voidptr) argOps
        let right = take 2 (argVOps ++ repeat nullptr)
        call funp . zip (ctx:right) $ repeat []
        --cgena [] (CNumber 42)



createClosure :: (MonadIRBuilder m, MonadModuleBuilder m) => (String -> Operand) -> [String] -> [String] -> m Operand
createClosure localArgs frees contextFrees = do
        ctx <- alloca (voidptr) (Just . consti32 . fromIntegral $ Data.List.length frees) 0
--        ctx <- alloca (context frees) Nothing 0
        forM (zip [0..] frees) $ \(i, free) -> do
                var <- cgena localArgs contextFrees (CVariable free)
                ctxPos <- gep ctx [consti32 i]
                store ctxPos 0 var
        bitcast ctx voidptr


cgena :: (MonadIRBuilder m, MonadModuleBuilder m) => (String -> Operand) -> [String] -> Aexp -> m Operand
cgena localArgs _ (CNumber n) = do
        var <- alloca num Nothing 0
        int32 n >>= store var 0
        bitcast var voidptr

cgena localArgs frees (CAbstraction fname innerFrees _ _) = do
        -- traceM ("cgena " <> fname)
        fptr <- demonicfunref $ codename fname
        ctx <- createClosure localArgs innerFrees frees
        cl <- alloca closure Nothing 0
        fpos <- gep cl [consti32 0, consti32 0]
        cpos <- gep cl [consti32 0, consti32 1]
        store fpos 0 fptr
        store cpos 0 ctx
        voidcl <- bitcast cl voidptr 
        --traceM ("cgena " <> fname <> " -> " <> show cl <> " -> " <> show voidcl)
        return voidcl

cgena localArgs frees (CVariable x) = case elemIndex x frees of
        Just n -> do
                traceM $ x <> " is free"
                let ctx = localArgs "ctx"
                typedCtx <- bitcast ctx $ ptr (context frees)
                ctxPos <- gep typedCtx [consti32 0, consti32 $ fromIntegral n]
                load ctxPos 0
        Nothing -> trace (x <> " is bound") . return $ localArgs x
cgena localArgs frees Top = cgena localArgs frees (CAbstraction "ctop" [] ["retval"] (CApplication (CNumber 0) []))



codegenTop :: Cexp -> ModuleBuilder ()
codegenTop cexp = do
        function 
                (codename "main")
                []
                num
                $ \[] -> mdo
                        _entry <- block `named` "entry"
                        resvoidptr <- cgenc (\x -> error $ "Variable " <> x <> " cannot be defined because it occurs at the top level") [] cexp
                        resnumptr  <- bitcast resvoidptr (ptr num)
                        res <- load resnumptr 0
                        ret res
        return ()


        
codegenAbs :: Aexp -> ModuleBuilder ()
codegenAbs (CAbstraction (stripPrefix "operator" -> Just f) frees args Bottom) = do
        function 
                (codename ("operator" <> f))
                sig
                voidptr
                $ \[ctxv, b, cont] -> mdo
                        let largs = zip (map snd sig) [ctxv, b, cont]
                        let largLookup = \x -> maybe (error (x <> " is not defined")) id $ lookup (ParameterName $ encodename x) largs
                        _entry <- block `named` "entry"
                        aptr <- cgena largLookup frees (CVariable "a")
                        bptr <- cgena largLookup frees (CVariable "b")

                        aa <- bitcast aptr (ptr num) >>= \p -> load p 0
                        bb <- bitcast bptr (ptr num) >>= \p -> load p 0

                        res <- primBinOp f aa bb 
                        var <- alloca num Nothing 0
                        store var 0 res
                        resptr <- bitcast var voidptr

                        leftCast <- bitcast cont $ ptr closure
                        funpPos <- gep leftCast [consti32 0, consti32 0]
                        ctxPos  <- gep leftCast [consti32 0, consti32 1]

                        funp <- load funpPos 0
                        innerctx  <- load ctxPos 0
                        call funp [(innerctx, []), (resptr, []), (nullptr, [])] >>= ret
        return ()
        where
                sig = take 3 $ [(voidptr, ParameterName $ encodename "ctx")] <> toSig args <> repeat (voidptr, "unused")
        

codegenAbs (CAbstraction _ frees args Bottom) = error "Unknown bottom function"

codegenAbs (CAbstraction name frees args cexp) = do
        function 
                (codename name)
                sig
                voidptr
                $ \args -> mdo
                        let largs = zip (map snd sig) args
                        let largLookup = \x -> maybe (error (x <> " is not defined")) id $ lookup (ParameterName $ encodename x) largs
                        _entry <- block `named` "entry"
                        cgenc largLookup frees cexp >>= ret 
        return ()
        where
                sig = take 3 $ [(voidptr, ParameterName $ encodename "ctx")] <> toSig args <> repeat (voidptr, "unused")

codegenCTop :: ModuleBuilder ()
codegenCTop = do
        function 
                (codename "ctop") 
                [(voidptr, "unused"), (voidptr, "res"), (voidptr, "unused")]
                voidptr
                $ \[_, res, _] -> mdo
                        _entry <- block `named` "entry"
                        resCast <- bitcast res $ ptr num
                        answer <- load resCast 0 
                        exit <- primref "exit"
                        call exit [(answer,[])] >> ret nullptr
        return ()


declareFun :: Aexp -> ModuleBuilder ()
declareFun (CAbstraction name _ args _) = extern (codename name) [voidptr, voidptr, voidptr] voidptr  >> return ()

declareExterns :: ModuleBuilder ()
declareExterns = do
        forM primExternal $ \(name, retty, args, vararg) -> extern name args retty  
        return ()

primExternal :: [(Name, Type, [Type], Bool)]
primExternal = [(codename "exit", voidptr, [AST.i32], False)]

primExternalType :: [(Name, Type)]
primExternalType = map (\(name, retty, argty, vararg) -> (codename "exit", AST.FunctionType retty argty vararg)) primExternal 


primBinOp :: MonadIRBuilder m => String -> (Operand -> Operand -> m Operand)
primBinOp "+"   = add
primBinOp "-"   = sub
primBinOp "·" = mul
primBinOp "/"   = sdiv
primBinOp "∧" = I.and
primBinOp "∨"  = I.or




codegen :: Cexp -> IO Module
codegen topExp = withContext $ \ctx -> do
        --putStrLn . show $ ppllvm newast
        LVM.withModuleFromAST ctx newast $ \m -> do --(trace (LT.unpack $ pShow newast) newast) $ \m -> do
                traceM "lowered"
                verify m
                traceM "verif"
                llstr <- LVM.moduleLLVMAssembly m
                BS.putStrLn llstr
                LVM.writeLLVMAssemblyToFile (LVM.File "a.ll") m
                --    withHostTargetMachine $ \hostMachine -> do
                --        writeObjectToFile hostMachine (File "a.out") m
                return newast
        where
                modn = declareExterns >> codegenCTop >> mapM declareFun fns >> mapM codegenAbs fns >> codegenTop topExp
                --modn = codegenCTop >> mapM declareFun fns >> codegenTop topExp
                fns = getCAbstractions topExp
                newast = buildModule "test" modn
