module CPS where

import Syntax

import Control.Monad.State

data Cexp = CApplication Aexp [Aexp] deriving (Show)
data Aexp = CAbstraction String [String] [String] Cexp | CVariable String | CNumber Integer | Top deriving (Show)


getCAbstractions :: Cexp -> [Aexp]
getCAbstractions (CApplication left right) = getCAbstractions' left <> concatMap getCAbstractions' right
        where 
                getCAbstractions' aexp = case aexp of
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
        app1 <- (t t1 (CVariable k))
        let free = freeVarsC [s,k] app1
        return $ CAbstraction ("f" <> show abstractionTag) free [s,k] app1
m (Number n) = return $ CNumber n

t :: Term -> Aexp -> CPSTransformer Cexp
t (Application f e) cont = do
        c1 <- fresh
        c2 <- fresh
        abstractionTag1 <- fresh
        abstractionTag2 <- fresh
        let fs = show c1
        let es = show c2
        let app0 = (CApplication (CVariable fs) [CVariable es, cont])
        let free0 = freeVarsC [es] app0
        let ae0 = CAbstraction ("f" <> show abstractionTag1) free0 [es]  app0

        ce <- t e ae0
        let free1 = freeVarsC [fs] ce
        let ae1 = CAbstraction ("f" <> show abstractionTag2) free1 [fs] ce
        t f ae1
t t1 cont = do
        ae <- m t1
        return $ CApplication cont [ae]

transformCPS :: Term -> Cexp
transformCPS t1 = evalState (runTransform m) emptyState
        where
                m = t t1 Top
