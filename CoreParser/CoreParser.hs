import BasicParsers
import Definitions
import ArithmeticParser
import System.IO

type Name = String

data Expr a = EVar Name         --Variables
 | ENum Int                     --Numbers
 | EConstr Int Int              --Constructor tag arity
 | EAp (Expr a) (Expr a)        --Applications
 | ELet IsRec [Def a] (Expr a)  --Let(rec) expressions
 | ECase (Expr a) [Alter a]     --Case expression
 | ELam [a] (Expr a)            --Lambda abstractions
 deriving Show

type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name,[a],Expr a)
type CoreScDefn = ScDefn Name
type Def a = (a,Expr a)         --for let
type Alter a = (Int,[a],Expr a) --for case

data IsRec = NonRecursive | Recursive --to distinguish let/letrec
 deriving Show


parseProg::Parser(Program Name)
parseProg = do p<-parseScDef
               do character ';'
                  ps<-parseProg
                  return (p:ps)
                <|> return [p]

parseScDef::Parser(ScDefn Name)
parseScDef = do v<-parseVar
                pf<-many parseVar
                character '='
                body<-parseExpr
                return (v,pf,body)


parseExpr::Parser(Expr Name)
parseExpr = do symbol "let" --let
               dnf<-parseDef
               dnfs<-many (do character ';'
                              parseDef)
               symbol "in"
               el<-parseExpr
               return (ELet NonRecursive (dnf:dnfs) el)
             <|> do symbol "letrec" --letrec
                    dnf<-parseDef
                    dnfs<-many (do character ';'
                                   parseDef)
                    symbol "in"
                    el<-parseExpr
                    return (ELet Recursive (dnf:dnfs) el)
             <|> do symbol "case" --case
                    ec<-parseExpr
                    symbol "of"
                    alt<-parseAlt
                    alts<-many (do character ';'
                                   parseAlt)
                    return (ECase ec (alt:alts))
             <|> do character '\\' --lambda
                    vs<-some parseVar
                    character '.'
                    exp<-parseExpr
                    return (ELam vs exp)
             <|> do e1<-parseExpr1 --expr1
                    return e1

parseExpr1::Parser(Expr Name)
parseExpr1 = do e2<-parseExpr2
                character '|'
                e1<-parseExpr1
                return (EAp (EAp (EVar "|") e2) e1)
              <|> do e<-parseExpr2
                     return e

parseExpr2::Parser(Expr Name)
parseExpr2 = do e3<-parseExpr3
                character '&'
                e2<-parseExpr2
                return (EAp (EAp (EVar "&") e3) e2)
              <|> do e<-parseExpr3
                     return e

parseExpr3::Parser(Expr Name)
parseExpr3 = do e4_1<-parseExpr4
                op<-parseRelop
                e4_2<-parseExpr4
                return (EAp (EAp op e4_1) e4_2)
              <|> do e<-parseExpr4
                     return e

parseExpr4::Parser(Expr Name)
parseExpr4 = do e5<-parseExpr5
                character '+'
                e4<-parseExpr4
                return (EAp (EAp (EVar "+") e5) e4)
              <|> do e5_2<-parseExpr5
                     character '-'
                     e5_3<-parseExpr5
                     return (EAp (EAp (EVar "-") e5_2) e5_3)
              <|> do e<-parseExpr5
                     return e

parseExpr5::Parser(Expr Name)
parseExpr5 = do e6<-parseExpr6
                character '*'
                e5<-parseExpr5
                return (EAp (EAp (EVar "*") e6) e5)
              <|> do e6_2<-parseExpr6
                     character '/'
                     e6_3<-parseExpr6
                     return (EAp (EAp (EVar "/") e6_2) e6_3)
              <|> do e<-parseExpr6
                     return e

parseExpr6::Parser(Expr Name)
parseExpr6 = do aexps<-some parseAExpr
                return (applicate aexps)


applicate::[(Expr Name)]->(Expr Name)
applicate [e] = e
applicate es = (EAp (applicate (init es)) (last es))


parseAExpr::Parser(Expr Name)
parseAExpr = do v<-parseVar --variable
                return (EVar v)
              <|> do n<-integer --number
                     return (ENum n)
              <|> do symbol "Pack" --constructor
                     character '{'
                     a1<-natural
                     character ','
                     a2<-natural
                     character '}'
                     return (EConstr a1 a2)
              <|> do character '(' --parenthesis
                     e<-parseExpr
                     character ')'
                     return e

parseDef::Parser(Def Name)
parseDef = do v<-parseVar
              character '='
              e<-parseExpr
              return (v,e)

parseAlt::Parser(Alter Name)
parseAlt = do character '<'
              a<-natural
              character '>'
              vs<-many parseVar
              symbol "->"
              expr<-parseExpr
              return (a,vs,expr)

parseRelop::Parser(Expr Name)
parseRelop = do symbol "=="
                return (EVar "==")
              <|> do symbol "~="
                     return (EVar "~=")
              <|> do symbol ">"
                     return (EVar ">")
              <|> do symbol ">="
                     return (EVar ">=")
              <|> do symbol "<"
                     return (EVar "<")
              <|> do symbol "<="
                     return (EVar "<=")

parseVar::Parser(Name)
parseVar = do space
              v<-lowerLet
              vs<-many (do alphanum
                         <|> char '_')
              case (v:vs) of
               "case" -> empty
               "let" -> empty
               "letrec" -> empty
               "Pack" -> empty
               "in" -> empty
               "of" -> empty
               _ -> return (v:vs)

--for opening and reading input file

readF::IO String
readF = do inh<-openFile "input.txt" ReadMode
           prog<-readloop inh
           hClose inh
           return prog

main::IO(Program Name)
main = do inp<-readF
          return (comp (parse parseProg inp))

comp::[(Program Name,Name)]->Program Name
comp [] = error "no parse"
comp [(e,[])] = e
comp [(_,a)] = error ("doesn't use all input "++a)

readloop inh = do ineof<-hIsEOF inh
                  if ineof
                      then return []
                      else do
                             x<-hGetLine inh
                             xs<-readloop inh
                             return (x++xs)
