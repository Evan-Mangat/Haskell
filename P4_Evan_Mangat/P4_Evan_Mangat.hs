import Prelude
import System.IO
import System.Environment
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State.Lazy

type VarId = String


data Expr = CInt Int
    | CBool Bool
    | Var VarId
    | Plus Expr Expr
    | Minus Expr Expr
    | Equal Expr Expr
    | ITE Expr Expr Expr
    | Abs VarId Expr
    | App Expr Expr
    | LetIn VarId Expr Expr
    deriving (Eq, Ord, Read, Show)


data Type = TInt
          | TBool
          | TError
          | TVar Int
          | TArr Type Type
          deriving (Eq, Ord, Read, Show)

data Constraint = CEq Type Type
                | CError
                deriving (Eq, Ord, Read, Show)

type ConstraintSet = Set.Set Constraint
type ConstraintList = [Constraint]

type Substitution = Map.Map Type Type



type RelabelState a = State (Map.Map Int Int) a

relabel :: Type -> Type
relabel t = evalState (go t) Map.empty
  where
    go :: Type -> RelabelState Type
    go TInt = return TInt
    go TBool = return TBool
    go TError = return TError
    go (TVar x) = do m <- get
                     case Map.lookup x m of
                        Just v -> return (TVar v)
                        Nothing -> do let n = 1 + Map.size m
                                      put (Map.insert x n m)
                                      return (TVar n)
    go (TArr t1 t2) = do t1' <- go t1
                         t2' <- go t2
                         return (TArr t1' t2')

-- Step 1
type Env = Map.Map VarId Type
type InferState a = State Int a


-- Step 2
getFreshTVar :: InferState Type
getFreshTVar = do
  n <- get
  put (n+1)
  return (TVar n)

-- Step 3
infer :: Env -> Expr -> InferState (Type, ConstraintSet)
infer g (CInt _) = return (TInt, Set.empty)
infer g (CBool _) = return (TBool, Set.empty)
infer g (Var x) = case Map.lookup x g of
                    Just t -> return (t, Set.empty)
                    Nothing -> return (TError, Set.singleton CError)
infer g (Plus e1 e2) = inferBinaryOp g TInt e1 e2
infer g (Minus e1 e2) = inferBinaryOp g TInt e1 e2
infer g (Equal e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c3 = Set.singleton (CEq t1 t2)
  return (TBool, Set.unions [c1, c2, c3])
infer g (ITE e1 e2 e3) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  (t3, c3) <- infer g e3
  let c4 = Set.fromList [CEq t1 TBool, CEq t2 t3]
  return (t2, Set.unions [c1, c2, c3, c4])
infer g (Abs x e) = do
  y <- getFreshTVar
  (t, c) <- infer (Map.insert x y g) e
  return (TArr y t, c)
infer g (App e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  t <- getFreshTVar
  let c3 = Set.singleton (CEq t1 (TArr t2 t))
  return (t, Set.unions [c1, c2, c3])
infer g (LetIn x e1 e2) = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer (Map.insert x t1 g) e2
  return (t2, Set.unions [c1, c2])

inferBinaryOp :: Env -> Type -> Expr -> Expr -> InferState (Type, ConstraintSet)
inferBinaryOp g t e1 e2 = do
  (t1, c1) <- infer g e1
  (t2, c2) <- infer g e2
  let c3 = Set.fromList [CEq t1 t, CEq t2 t]
  return (t, Set.unions [c1, c2, c3])

-- Step 4
inferExpr :: Expr -> (Type, ConstraintSet)
inferExpr e = evalState (infer Map.empty e) 0

-- Step 5
toCstrList :: ConstraintSet -> ConstraintList
toCstrList = Set.toList

-- Step 6
applySub :: Substitution -> Type -> Type
applySub _ TInt = TInt
applySub _ TBool = TBool
applySub _ TError = TError
applySub sigma (TVar x) = Map.findWithDefault (TVar x) (TVar x) sigma
applySub sigma (TArr t1 t2) = TArr (applySub sigma t1) (applySub sigma t2)


-- Step 7
applySubToCstrList :: Substitution -> ConstraintList -> ConstraintList
applySubToCstrList _ [] = []
applySubToCstrList sigma (c:cs) = applySubToCstr sigma c : applySubToCstrList sigma cs
  where applySubToCstr :: Substitution -> Constraint -> Constraint
        applySubToCstr _ CError = CError
        applySubToCstr sigma (CEq t1 t2) = CEq (applySub sigma t1) (applySub sigma t2)

-- Step 8
composeSub :: Substitution -> Substitution -> Substitution
composeSub s1 s2 = Map.map (applySub s1) s2 `Map.union` s1
  where
    applySub s (TVar x) = Map.findWithDefault (TVar x) (TVar x) s
    applySub s (TArr t1 t2) = TArr (applySub s t1) (applySub s t2)
    applySub _ t = t

-- Step 9
tvars :: Type -> Set.Set Type
tvars TInt = Set.empty
tvars TBool = Set.empty
tvars TError = Set.empty
tvars (TVar x) = Set.singleton (TVar x)
tvars (TArr t1 t2) = Set.union (tvars t1) (tvars t2)

-- Step 10
tvarsIndices :: Set.Set Type -> Set.Set Int
tvarsIndices typeVars = Set.foldr (\(TVar i) acc -> Set.insert i acc) Set.empty typeVars

unify :: ConstraintList -> Maybe Substitution
unify [] = Just Map.empty
unify [CError] = Nothing
unify (CEq t1 t2 : cs)
  | t1 == t2 = unify cs
  | TVar x <- t1, not (x `Set.member` tvarsIndices (tvars t2)) = unify1 t1 t2
  | TVar x <- t2, not (x `Set.member` tvarsIndices (tvars t1)) = unify1 t2 t1
  | TArr t11 t12 <- t1, TArr t21 t22 <- t2 = unify (CEq t11 t21 : CEq t12 t22 : cs)
  | otherwise = Nothing
  where
    unify1 tvar@(TVar x) t = do
      s <- unify (applySubToCstrList (Map.singleton tvar t) cs)
      return (Map.insert tvar t s)
unify (_ : _) = Nothing

-- Step 11
typing :: Expr -> Maybe Type
typing expr =
  case inferExpr expr of
    (ty, cstrs) ->
      case unify (Set.toList cstrs) of
        Just subst -> Just (applySub subst ty)
        Nothing -> Nothing


-- Step 12
typeInfer :: Expr -> String
typeInfer e = case typing e of
  Just v -> case v of 
            TError -> "Type Error"
            _ -> show (relabel v)
  Nothing -> "Type Error"


-- Step 13
readExpr :: String -> Expr
readExpr e = read e :: Expr


main :: IO ()
main = do
    arg <- getArgs
    input <- readFile (head arg)
    let expressions = lines input
        results = Prelude.map readExpr expressions
        results2 = Prelude.map typeInfer results
    mapM_ putStrLn results2    
