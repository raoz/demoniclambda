module CPS where

import Syntax

import Control.Monad.State

data Cexp = CApplication Aexp [Aexp] | Bottom deriving (Show)
data Aexp = CAbstraction String [String] [String] Cexp | CVariable String | CNumber Integer | CBool Bool | Top deriving (Show) 


getCAbstractions :: Cexp -> [Aexp]
getCAbstractions (CApplication left right) = getCAbstractions' left <> concatMap getCAbstractions' right
        where 
                getCAbstractions' aexp = case aexp of
                        (CAbstraction _ _ _ Bottom) -> [aexp]
                        (CAbstraction _ _ _ cexp) -> [aexp] <> getCAbstractions cexp
                        _ -> []

--instance Show Cexp where
--        show (CApplication f args) = "(" <> (unwords ([show f] <> map show args)) <> ")"
--instance Show Aexp where
--        show (CAbstraction xs t1) = "(λ" <> (unwords (map show xs)) <> "." <> show t1
--        show (CVariable s) = s
--        show (CNumber n) = show n
--        show Top = "⊤"

emptyState :: CPSState
emptyState = TransformState 0

data CPSState
  = TransformState {
          freshNum :: Int
  } 

newtype CPSTransformer a = CPSTransformer { runTransform :: State CPSState a }
  deriving (Functor, Applicative, Monad, MonadState CPSState )

fresh :: CPSTransformer Int
fresh = do
        i <- gets freshNum
        modify $ \s -> s { freshNum = i + 1 }
        return $ i 


freeVarsC :: [String] -> Cexp -> [String]
freeVarsC bound (CApplication f xs) = filter (`notElem` bound) $ ((freeVarsA bound f) <> concatMap (freeVarsA bound) xs)
freeVarsC bound (Bottom) = []

freeVarsA :: [String] -> Aexp -> [String]
freeVarsA bound (CAbstraction _ _ xs c) = filter (`notElem` newbound) $ freeVarsC newbound c
        where
                newbound = bound <> xs
freeVarsA bound (CNumber _) = []
freeVarsA bound (CVariable s) = [s]
freeVarsA bound (Top) = []

m :: Term -> CPSTransformer Aexp
m (Variable s) = return $ CVariable s
m (Abstraction s t1) = do 
        c <- fresh
        abstractionTag <- fresh
        let k = show c
        app1 <- (tc t1 $ CVariable k)
        let free = freeVarsC [s,k] app1
        return $ CAbstraction ("f" <> show abstractionTag) free [s,k] app1
m (Number n) = return $ CNumber n
m (Boolean True) = return $ CNumber 1
m (Boolean False) = return $ CNumber 0


-- TODO: cleanup repetition


tk :: Term -> (Aexp -> CPSTransformer Cexp) -> CPSTransformer Cexp
tk (BinOp f a b) k = do
        c0 <- fresh
        abstractionTagC <- fresh

        let rv = show c0
        inner <- k $ CVariable rv

        let freec = freeVarsC [rv] inner
        let cont = CAbstraction ("f" <> show abstractionTagC) freec [rv] $ inner

        tk a (\aa -> tk b (\bb -> do
                a1 <- ("f" <>) . show <$> fresh
                c1 <- show <$> fresh
                let innerAbs = CAbstraction ("operator" <> f) ["a"] ["b", "op" <> f <> "k"] Bottom
                return $ CApplication (CAbstraction a1 [] ["a", c1] $ CApplication innerAbs [bb, CVariable (c1)]) [aa, cont]))

tk (Application f e) k = do
        c1 <- fresh
        abstractionTagC <- fresh
        let rv = show c1

        inner <- k $ CVariable rv

        let freec = freeVarsC [rv] inner
        let cont = CAbstraction ("f" <> show abstractionTagC) freec [rv] $ inner


        tk f (\ff -> tk e (\ee -> return $ CApplication ff [ee, cont]))

tk t1 k = do
        ae <- m t1
        k ae

tc :: Term -> Aexp -> CPSTransformer Cexp
tc (BinOp f a b) cont = do
        tk a (\aa -> tk b (\bb -> do
                a1 <- ("f" <>) . show <$> fresh
                c1 <- show <$> fresh
                let innerAbs = CAbstraction ("operator" <> f) ["a"] ["b", "op" <> f <> "k"] Bottom
                return $ CApplication (CAbstraction a1 [] ["a", c1] $ CApplication innerAbs [bb, CVariable (c1)]) [aa, cont]))



tc (Application f e) cont = tk f (\ff -> tk e (\ee -> return $ CApplication ff [ee, cont]))
tc t1 cont = do
        a <- m t1 
        return $ CApplication cont [a]





transformCPS :: Term -> Cexp
transformCPS t1 = evalState (runTransform m) emptyState
        where
                m = tc t1 Top
