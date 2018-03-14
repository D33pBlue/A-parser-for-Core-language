
type State = Int
newtype ST a = S (State -> (a,State))

app::ST a -> State -> (a,State)
app (S g) s = g s

-- instance Functor ST where
--  --fmap :: (a->b) -> ST a -> ST b
--  fmap g st = S(\s->let (x,s') = app st s in (g x,s'))
--
-- instance Applicative ST where
--  --pure :: a -> ST a
--  pure x = S(\s->(x,s))
--  --(<*>) :: ST(a->b) -> ST a -> ST b
--  stf <*> stx = S(\s->
--   let (f,s') = app stf s
--       (x,s'') = app stx s' in (f x,s''))
--
-- instance Monad ST where
--  return = pure
--  --(>>=) :: ST a -> (a->ST b) -> ST b
--  stx >>= g = S(\s->
--   let (x,s') = app stx s in app (g x) s')


---------------------------------------------------------------

instance Monad ST where
 st >>= f = S(\s->
  let (x,s') = app st s in app (f x) s')


instance Applicative ST where
 --pure :: a -> ST a
 pure x = S(\s->(x,s))
 --(<*>)::ST(a->b) -> ST a -> ST b
 stf <*> stx = do f<-stf
                  x<-stx
                  return (f x)

instance Functor ST where
 --fmap :: (a->b) -> ST a -> ST b
 fmap g st = do x<-st
                return (g x)
