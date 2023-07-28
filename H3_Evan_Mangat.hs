-- Question 1 -----------------------------------------------------
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
listZip :: List a -> List b -> List(a,b)
listZip xs Empty = Empty
listZip Empty ys = Empty
listZip (Cons x xs) (Cons y ys) = (x, y) `Cons` listZip xs ys

-- Question 2 -----------------------------------------------------
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singular :: a -> Tree a
singular x = Node x EmptyTree EmptyTree

insert  :: (Ord a) => a -> Tree a -> Tree a
insert  x EmptyTree = singular x
insert  x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (insert x left) right
    | x > a = Node a left (insert x right)

-- Question 3 -----------------------------------------------------
data Nat = Zero | Succ Nat deriving (Show, Read, Eq, Ord)
natPlus :: Nat -> Nat -> Nat
natPlus Zero a = a
natPlus a Zero = a
natPlus (Succ a) (Succ b) = natPlus (Succ(Succ a)) b

natMult :: Nat -> Nat -> Nat
natMult Zero a = Zero
natMult (Succ Zero) a = a
natMult (Succ b) a = natMult b (natPlus a a)

-- Question 4 -----------------------------------------------------
-------------------------------------------------------------------
{- Function Version to help visualize/brainstorm:
equal :: Eq a => Tree a -> Tree a -> Bool
equal EmptyTree EmptyTree = True
equal EmptyTree (Node _ _ _) = False
equal (Node _ _ _) EmptyTree = False
equal (Node a l r) (Node b l1 r1) = (a == b) && (equal l l1) && (equal r r1)
-}
-------------------------------------------------------------------
-- Actual Answer:
instance Eq a => Eq (Tree a) where
    EmptyTree  == EmptyTree = True
    EmptyTree == (Node {}) = False
    (Node {}) == EmptyTree = False
    (Node a l r) == (Node b l1 r1) = a==b && l==l1 && r==r1

-- Question 5 -----------------------------------------------------
data AssocList k v = ALEmpty | ALCons k v (AssocList k v) deriving (Show)

instance Functor (AssocList k) where
    fmap :: (a -> b) -> AssocList k a -> AssocList k b
    fmap f (ALCons k v c) = ALCons k (f v) (fmap f c) 
    fmap _ ALEmpty = ALEmpty

