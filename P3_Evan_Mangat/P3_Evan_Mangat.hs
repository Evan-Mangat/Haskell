import Prelude
import System.IO
import System.Environment
import Data.Map.Strict as Map


data Type = TInt
    | TBool
    | TArr Type Type
    deriving (Eq, Ord, Read, Show)

type VarId = String

data Expr = CInt Int
    | CBool Bool
    | Var VarId
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | ITE Expr Expr Expr
    | Abs VarId Type Expr
    | App Expr Expr
    | LetIn VarId Type Expr Expr
    deriving (Eq, Ord, Read, Show)

-- Step 1
type Env = Map.Map VarId Type


-- Step 2
typingArith :: Maybe Type -> Maybe Type -> Maybe Type
typingArith (Just TInt)(Just TInt) = Just TInt
typingArith _ _ = Nothing

-- Step 3
typingEq :: Maybe Type -> Maybe Type -> Maybe Type
typingEq (Just TInt)(Just TInt) = Just TBool
typingEq (Just TBool)(Just TBool) = Just TBool
typingEq _ _ = Nothing

-- Step 4
typing :: Env -> Expr -> Maybe Type
typing env (CInt _) = Just TInt
typing env (CBool _) = Just TBool
typing env (Var x) = Map.lookup x env
typing env (Plus e1 e2) =
  case (typing env e1, typing env e2) of
    (Just TInt, Just TInt) -> Just TInt
    _ -> Nothing
typing env (Minus e1 e2) =
  case (typing env e1, typing env e2) of
    (Just TInt, Just TInt) -> Just TInt
    _ -> Nothing
typing env (Equal e1 e2) =
  case (typing env e1, typing env e2) of
    (Just t1, Just t2) | t1 == t2 -> Just TBool
    _ -> Nothing
typing env (ITE e1 e2 e3) =
  case (typing env e1, typing env e2, typing env e3) of
    (Just TBool, Just t2, Just t3) | t2 == t3 -> Just t2
    _ -> Nothing
typing env (Abs x t e) = TArr t <$> typing (Map.insert x t env) e
typing env (App e1 e2) =
  case typing env e1 of
    Just (TArr t1 t2) -> if typing env e2 == Just t1 then Just t2 else Nothing
    _ -> Nothing
typing env (LetIn x t e1 e2) =
  case (typing env e1, typing (Map.insert x t env) e2) of
    (Just t1, Just t2) -> Just t2
    _ -> Nothing

-- Step 5
readExpr :: String -> Expr
readExpr e = read e :: Expr

-- Step 6
typeCheck :: Expr -> String
typeCheck e = case typing Map.empty e of
  Just v -> show v
  Nothing -> "Type Error"

-- Step 7

main :: IO ()
main = do
    arg <- getArgs
    input <- readFile (head arg)
    let expressions = lines input
        results = Prelude.map readExpr expressions
        results2 = Prelude.map typeCheck results
    mapM_ putStrLn results2    
