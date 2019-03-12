module Codegen where

import CPS
import Control.Monad.State

import LLVM.Module
import LLVM.Context

import Control.Monad.Except

import Data.ByteString.Short
import qualified Data.ByteString.Char8 as BS


import Data.Word
import Data.String
import Data.List
import Data.Function

import qualified Data.Map as M


import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

packStr :: String -> ShortByteString
packStr = toShort . encodeUtf8 . T.pack

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs <> [d] }

define ::  Type -> ShortByteString -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

num :: Type
num = IntegerType 64

closure :: Aexp -> Type
closure = undefined

context :: [String] -> Type
context frees = ArrayType (fromIntegral $ Data.List.length frees) num



-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = M.Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case M.lookup nm ns of
    Nothing -> (nm,  M.insert nm 1 ns)
    Just ix -> (nm <> (packStr $ show ix), M.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------



type SymbolTable = [(ShortByteString, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: M.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , termi  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ M.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " <> (show l)

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) M.empty [] 1 0 M.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

freshW :: Codegen Word
freshW = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen (Operand)
instr ins = do
  n <- freshW
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { termi = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: ShortByteString -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms

  modify $ \s -> s { blocks = M.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = M.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case M.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " <> show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: ShortByteString -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] <> lcls }

getvar :: ShortByteString -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " <> show var

-------------------------------------------------------------------------------

-- References
local ::  Name -> Operand
local = LocalReference num

global ::  Name -> C.Constant
global = C.GlobalReference num

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference num

-- Arithmetic and Constants
--fadd :: Operand -> Operand -> Codegen Operand
--fadd a b = instr $ FAdd NoFastMathFlags a b []
--
--fsub :: Operand -> Operand -> Codegen Operand
--fsub a b = instr $ FSub NoFastMathFlags a b []
--
--fmul :: Operand -> Operand -> Codegen Operand
--fmul a b = instr $ FMul NoFastMathFlags a b []
--
--fdiv :: Operand -> Operand -> Codegen Operand
--fdiv a b = instr $ FDiv NoFastMathFlags a b []
--
--fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
--fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []












cgena :: Aexp -> Codegen AST.Operand
cgena (CNumber n) = return . cons $ C.Int 64 n

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (num, AST.Name $ packStr x))


codegenTop :: Cexp -> LLVM ()
codegenTop (CApplication f ts) = do
        define num "main" [] blks
        where
                blks = createBlocks $ execCodegen $ do
                        entry <- addBlock entryBlockName
                        setBlock entry
                        cgena (CNumber 42) >>= ret
codegenAbs :: Aexp -> LLVM ()
codegenAbs (CAbstraction name frees args ts) = do
        define num (packStr name) ([(context frees, "ctx")] <> toSig args) blks
        where
                blks = createBlocks $ execCodegen $ do
                        entry <- addBlock entryBlockName
                        setBlock entry
                        cgena (CNumber 42) >>= ret





codegen :: AST.Module -> Cexp -> IO AST.Module
codegen mod topExp = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BS.putStrLn llstr
    return newast
  where
    modn = codegenTop topExp >> mapM codegenAbs fns
    fns = getCAbstractions topExp
    newast = runLLVM mod modn
