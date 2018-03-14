
instance Functor ((->) a) where
 --fmap :: (b->c)->(a->b)->(a->c)
 fmap = (.)

instance Applicative ((->) a) where
 --pure :: b -> (a->b)
 pure = const
 --(<*>):: (a->b->c) -> (a->b) -> (a->c)
 g <*> h = \x -> (g x) (h x)

instance Monad ((->) a) where
 return = pure
 --(>>=):: (a->b) -> (b -> a->c) -> (a->c)
 g >>= h = \x -> g (h x) x
