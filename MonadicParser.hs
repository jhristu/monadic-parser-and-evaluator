module MonadicParser where

import TypesAndTerms
import Data.Char

newtype Parser a = Parser { con::(String -> [(a, String)]) }

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser p) >>= f = 
        Parser (\s -> concat [con (f v) o | (v, o) <- p s])
    -- return :: a -> Parser a
    return v = Parser (\s -> [(v, s)])

-- pand p1 p2 : applies p1 and p2 and combines the results
pand :: Parser a -> Parser a -> Parser a
pand (Parser p1) (Parser p2) = Parser (\s -> p1 s ++ p2 s)

-- por p1 p2 : applies p1, if fails, applies and returns p2
por :: Parser a -> Parser a -> Parser a
por (Parser p1) (Parser p2) = Parser (\s -> case p1 s of
                                              [] -> p2 s
                                              [(v, o)] -> [(v, o)])

-- pstar p : parser star matches 0 or more instances of p
pstar :: Parser a -> Parser [a]
pstar p = por (do x <- p
                  xs <- pstar p
                  return (x:xs)) (return [])

-- pplus p : parser plus matches 1 or more instances of p
pplus :: Parser a -> Parser [a]
pplus p = do x <- p
             xs <- pstar p
             return (x:xs)

failure :: Parser a
failure = Parser (\s -> [])

single :: Parser Char
single = Parser (\s -> case s of
                       [] -> []
                       (x:xs) -> [(x, xs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do x <- single
               if p x then return x else failure

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

spaces :: Parser String
spaces = pplus (satisfy (\x -> (x == ' ') || (x == '\t') || (x == '\n')))

comment :: Parser String
comment = do string "--"
             xs <- pstar (satisfy (\x -> x /= '\n'))
             return xs

-- rwcb p : removes whitespace and comments before applying p
rwcb :: Parser a -> Parser a
rwcb p = do pstar (spaces `por` comment)
            v <- p
            return v

-- rwca p : removes whitespace and comments after applying p
rwca :: Parser a -> Parser a
rwca p = do v <- p
            pstar (spaces `por` comment)
            return v

paren :: Parser a -> Parser a
paren p = do rwca (char '(')
             x <- p
             rwca (char ')')
             return x

tmvarparser :: Parser Tm
tmvarparser = rwca (do v <- varparser
                       return (TmVar v))

tmabsparser :: Parser Tm
tmabsparser = rwca (do rwca (satisfy (== '\\'))
                       x <- varparser
                       rwca (satisfy (== ':'))
                       ty <- typarser
                       rwca (satisfy (== '.'))
                       tm <- tmparser
                       return (TmAbs x ty tm))

tmfixparser :: Parser Tm
tmfixparser = rwca (do rwca (string "fix")
                       tm <- tmparser
                       return (TmFix tm))

tmappparser :: Parser Tm
tmappparser = rwca (do tm <- tmsimpleparser
                       tms <- (pstar tmsimpleparser)
                       return (foldl (\tm1 tm2 -> (TmApp tm1 tm2))
                                     tm tms))

tmtrueparser :: Parser Tm
tmtrueparser = rwca (do string "true"
                        return TmTrue)

tmfalseparser :: Parser Tm
tmfalseparser = rwca (do string "false"
                         return TmFalse)

tmifparser :: Parser Tm
tmifparser = rwca (do rwca (string "if")
                      tm1 <- tmparser
                      rwca (string "then")
                      tm2 <- tmparser
                      rwca (string "else")
                      tm3 <- tmparser
                      return (TmIf tm1 tm2 tm3))

tmzeroparser :: Parser Tm
tmzeroparser = rwca (do string "zero"
                        return TmZero)

tmsuccparser :: Parser Tm
tmsuccparser = rwca (do rwca (string "succ")
                        tm <- tmparser
                        return (TmSucc tm))

tmpredparser :: Parser Tm
tmpredparser = rwca (do rwca (string "pred")
                        tm <- tmparser
                        return (TmPred tm))

tmiszeroparser :: Parser Tm
tmiszeroparser = rwca (do rwca (string "iszero")
                          tm <- tmparser
                          return (TmIsZero tm))

tmletparser :: Parser Tm
tmletparser = rwca (do rwca (string "let")
                       x <- varparser
                       rwca (string ":")
                       ty <- typarser
                       rwca (string "=")
                       tm1 <- tmparser
                       rwca (string "in")
                       tm2 <- tmparser
                       return (TmApp (TmAbs x ty tm2) tm1))

tmrefparser :: Parser Tm
tmrefparser = rwca (do rwca (string "ref")
                       tm <- tmsimpleparser
                       return (TmRef tm))

tmderefparser :: Parser Tm
tmderefparser = rwca (do rwca (string "!")
                         tm <- tmsimpleparser
                         return (TmDeref tm))

tmassignparser :: Parser Tm
-- tmassignparser = rwca (do tm <- tmappparser
--                           tms <- (pstar (do (rwca (string ":="))
--                                             tms0 <- tmappparser
--                                             return tms0))
--                           return (foldr (\tm1 tm2 -> (TmAssign tm1 tm2))
--                                         tm tms))
tmassignparser = rwca (do tm1 <- tmappparser
                          (por (do rwca (string ":=")
                                   tm2 <- tmassignparser
                                   return (TmAssign tm1 tm2))
                           (return tm1)))

tmsimpleparser :: Parser Tm
tmsimpleparser = tmvarparser `pand`
                 tmabsparser `pand`
                 tmfixparser `pand`
                 paren tmparser `pand`
                 tmtrueparser `pand`
                 tmfalseparser `pand`
                 tmzeroparser `pand`
                 tmsuccparser `pand`
                 tmpredparser `pand`
                 tmiszeroparser `pand`
                 tmifparser `pand`
                 tmletparser `pand`
                 tmrefparser `pand`
                 tmderefparser

tmparser :: Parser Tm
tmparser = tmassignparser

keywords = ["fix", "true", "false", "if", "then", "else", "let", "in", "ref", "succ", "pred", "zero", "iszero"]

varparser :: Parser Var
varparser = rwca (do v <- pplus (satisfy isAlpha)
                     if (not (elem v keywords)) then return v else failure)
                     

tyboolparser :: Parser Ty
tyboolparser = rwca (do string "Bool"
                        return TyBool)

tyunitparser :: Parser Ty
tyunitparser = rwca (do string "Unit"
                        return TyUnit)

tyrefparser :: Parser Ty
tyrefparser = rwca (do rwca (string "Ref")
                       ty <- typarser
                       return (TyRef ty))

tynatparser :: Parser Ty
tynatparser = rwca (do string "Nat"
                       return TyNat)

tyarrowparser :: Parser Ty
tyarrowparser = rwca (do ty1 <- rwca tysimpleparser
                         (por (do rwca (string "->")
                                  ty2 <- rwca tyarrowparser
                                  return (TyArr ty1 ty2))
                          (return ty1)))

typarenparser :: Parser Ty
typarenparser = typarenparser

tysimpleparser :: Parser Ty
tysimpleparser = tyboolparser `pand`
                 tyunitparser `pand`
                 tyrefparser `pand`
                 tynatparser `pand`
                 paren typarser

typarser :: Parser Ty
typarser = tyarrowparser

parse :: String -> Tm
parse s = case con (rwcb tmparser) s of
            [] -> error "nothing could be parsed"
            ((tm, s):r) -> tm
--            ((tm, s):r) -> error ("could not parse the remainder: " ++ s)
