
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
 deriving Show

instance Functor Expr where
 --fmap:: (a->b) -> Expr a -> Expr b
 fmap _ (Val x) = Val x
 fmap g (Var x) = Var (g x)
 fmap g (Add x y) = Add (fmap g x) (fmap g y)

instance Applicative Expr where
 --pure :: a -> Expr a
 pure = Var
 --(<*>):: Expr(a->b) -> Expr a -> Expr b
 (Val x) <*> _ = Val x
 (Var g) <*> x = fmap g x
 (Add f g) <*> x = Add (f <*> x) (g <*> x)

instance Monad Expr where
 return = pure
 --(>>=):: Expr a -> (a->Expr b) -> Expr b
 (Val x) >>= f = Val x
 (Var x) >>= f = f x
 (Add x y) >>= f = Add (x >>= f) (y >>= f)

--------------------------------------------------------------

-- assign :: Int -> a -> a -> Expr a
-- assign n c v x | c==v = Val n
--                | otherwise = Var x

assign :: Char -> Expr Char
assign x = if x=='x' then (Val 3) else (Var x)

e = Add (Add (Var 'x') (Val 1)) (Var 'y')
e3 = e >>= assign

-- e2 = do x<-Var 'x'
--         y<-Var 'y'
--         z<-Add (Var x) (Val 1)
--         return (Add (Var z) (Var y))
