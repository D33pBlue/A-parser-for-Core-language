module Definitions where

newtype Parser a = P(String->[(a,String)])

parse::Parser a -> String -> [(a,String)]
parse (P p) s = p s

instance Functor Parser where
 -- fmap :: (a->b) -> Parser a -> Parser b
 fmap g p = P(\s -> case parse p s of
  [] -> []
  [(x,out)] -> [(g x,out)])

instance Applicative Parser where
 --pure :: a -> Parser a
 pure a = P(\s -> [(a,s)])
 --(<*>) :: Parser(a->b) -> Parser a -> Parser b
 pg <*> px = P(\s -> case parse pg s of
  [] -> []
  [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
 -- return :: a -> Parser a
 return = pure
 --(>>=):: Parser a -> (a -> Parser b) -> Parser b
 px >>= f = P(\s -> case parse px s of
  [] -> []
  [(x,out)] -> parse (f x) out)

class Applicative f => Alternative f where
 empty :: f a
 (<|>) :: f a -> f a -> f a
 many :: f a -> f [a]
 some :: f a -> f [a]

 many x = some x <|> pure []
 some x = pure (:) <*> x <*> many x

instance Alternative Parser where
 empty = P(\s -> [])
 --(<|>) :: Parser a -> Parser a -> Parser a
 pa <|> pb = P(\s -> case parse pa s of
  [] -> parse pb s
  [(v,out)] -> [(v,out)])
