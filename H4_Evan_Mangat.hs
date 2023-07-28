data ErrJst e j = Err e | Jst j deriving (Show)

-- Question 1 -----------------------------------------------------
instance Functor (ErrJst e) where
    fmap :: (a -> b) -> ErrJst e a -> ErrJst e b
    fmap f (Jst j) = Jst (f j)
    fmap f (Err e) = Err e

-- Question 2 -----------------------------------------------------
instance Applicative (ErrJst e) where
    pure = Jst
    Err e <*> _ = Err e
    (Jst f) <*> j = fmap f j

-- Question 3 -----------------------------------------------------
instance Monad (ErrJst e) where
    (>>=) :: ErrJst e a -> ( a -> ErrJst e b) -> ErrJst e b
    Err e >>= _ = Err e
    (Jst j) >>= f = f j

-- Question 4 -----------------------------------------------------
join :: Monad m => m (m a) -> m a
join m = m >>= id

-- Question 5 -----------------------------------------------------
data LTree a = Leaf a | LNode (LTree a) (LTree a) deriving (Show)
instance Foldable LTree where
 foldMap :: Monoid m => (a -> m) -> LTree a -> m
 foldMap f (Leaf a) = (f a)
 foldMap f (LNode l r) = mappend (foldMap f r) (foldMap f l) 
 