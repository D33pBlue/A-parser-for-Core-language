module BasicParsers where

import Data.Char
import Definitions

item::Parser Char
item = P(\s -> case s of
 [] -> []
 (x:xs) -> [(x,xs)])

sat::(Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit::Parser Char
digit = sat isDigit

lower::Parser Char
lower = sat isLower

upper::Parser Char
upper = sat isUpper

letter::Parser Char
letter = sat isAlpha

lowerLet::Parser Char
lowerLet = do x <- item
              if ((isAlpha x)||(x == '_')) then return x else empty

alphanum::Parser Char
alphanum = sat isAlphaNum

char::Char->Parser Char
char x = sat (==x)

string::String->Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)


ident::Parser String
ident = do x<-lower
           xs<- many alphanum
           return (x:xs)

nat::Parser Int
nat = do xs<-some digit
         return (read xs)

space::Parser ()
space = do many (sat isSpace)
           return ()

int::Parser Int
int = do char '-'
         n <- nat
         return (-n)
        <|> nat

token::Parser a -> Parser a
token p = do space
             v<-p
             space
             return v

identifier::Parser String
identifier = token ident


natural::Parser Int
natural = token nat

integer::Parser Int
integer = token int

symbol::String->Parser String
symbol xs = token (string xs)

character::Char->Parser Char
character xs = token (char xs)

nats::Parser [Int]
nats = do symbol "["
          n<-natural
          ns<-many (do symbol ","
                       natural)
          symbol "]"
          return (n:ns)
