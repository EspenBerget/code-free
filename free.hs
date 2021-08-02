-- Straight forward implementation
data AST = Value Int
         | Add AST AST
         | Mul AST AST
         deriving Show


expr :: AST
expr = Add
        (Value 2)
        (Mul
            (Value 3)
            (Value 5))

eval :: AST -> Int
eval (Value n)   = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- >>> eval expr
-- 17

-- FIX
newtype Fix f = Fix { unFix :: f (Fix f) }

foldFix :: Functor f => (f a -> a) -> Fix f -> a
foldFix f = go where go = f . fmap go . unFix 


data ASTFix r = ValueFix Int
              | AddFix r r
              | MulFix r r
              deriving Show

instance Functor ASTFix where
    fmap _ (ValueFix n) = ValueFix n
    fmap f (AddFix a b) = AddFix (f a) (f b)
    fmap f (MulFix a b) = MulFix (f a) (f b)


exprFix :: Fix ASTFix
exprFix = Fix (AddFix
                (Fix (ValueFix 2))
                (Fix (MulFix 
                        (Fix (ValueFix 3)) 
                        (Fix (ValueFix 5)))))


algebra :: ASTFix Int -> Int
algebra (ValueFix n)   = n
algebra (AddFix a1 a2) = a1 + a2
algebra (MulFix a1 a2) = a1 * a2


evalFix :: Fix ASTFix -> Int
evalFix = foldFix algebra  

-- >>> evalFix exprFix
-- 17

-- Free
data Free f a = Pure a | Free (f (Free f a))

showFree :: (Show a, Functor f, Traversable f) => Free f a -> IO ()
showFree (Pure a)  = print a
showFree (Free fs) = mapM_ showFree fs


instance Functor f => Functor (Free f) where
    fmap f (Pure a)  = Pure (f a)
    fmap f (Free fa) = Free (fmap (fmap f) fa) 


instance Functor f => Applicative (Free f) where
    pure             = Pure
    (Pure f)  <*> fa = fmap f fa
    (Free fs) <*> fa = Free (fmap (<*> fa) fs)


instance Functor f => Monad (Free f) where
    return          = pure 
    (Pure a)  >>= k = k a 
    (Free fs) >>= k = Free (fmap (>>= k) fs)


data ASTFree r = AddFree r r | MulFree r r 

instance Functor ASTFree where
    fmap f (AddFree a b) = AddFree (f a) (f b)
    fmap f (MulFree a b) = MulFree (f a) (f b)


foldFree :: Functor f => (f a -> a) -> Free f a -> a
foldFree _   (Pure a) = a
foldFree phi (Free m) = phi (foldFree phi <$> m)


exprF :: Free ASTFree Int
exprF = Free (AddFree
                (Pure 2)
                (Free (MulFree
                        (Pure 3)
                        (Pure 5))))


algebraF :: ASTFree Int -> Int
algebraF (AddFree a b) = a + b
algebraF (MulFree a b) = a * b


evalFree :: Free ASTFree Int -> Int 
evalFree = foldFree algebraF
