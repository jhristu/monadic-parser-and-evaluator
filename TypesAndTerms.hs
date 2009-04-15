module TypesAndTerms where

import Data.Char
type Var = String
type Loc = Int

data Tm = TmTrue
        | TmFalse
        | TmIf Tm Tm Tm
        | TmZero
        | TmSucc Tm
        | TmPred Tm
        | TmIsZero Tm
        | TmVar Var
        | TmAbs Var Ty Tm
        | TmApp Tm Tm
        | TmFix Tm
        | TmUnit
        | TmRef Tm
        | TmDeref Tm
        | TmAssign Tm Tm
        | TmLoc Loc
        | TmLet Var Tm Tm

instance Show Tm where
    show (TmTrue) = "true"
    show (TmFalse) = "false"
    show (TmIf tm1 tm2 tm3) = "if " ++ show tm1 ++ " then " ++ show tm2 ++ " else " ++ show tm3
    show (TmZero) = "0"
    show (TmSucc tm) = "succ " ++ show tm
    show (TmPred tm) = "pred " ++ show tm
    show (TmIsZero tm) = "iszero " ++ show tm
    show (TmVar v) = v
    show (TmAbs v ty tm) = "\\" ++ v ++ ":" ++ show ty ++ " -> " ++ show tm
    show (TmApp tm1 tm2) = "(" ++ show tm1 ++ " " ++ show tm2 ++ ")"
    show (TmFix tm) = "fix " ++ show tm
    show (TmUnit) = "unit"
    show (TmRef tm) = "ref " ++ show tm
    show (TmDeref tm) = "! " ++ show tm
    show (TmAssign tm1 tm2) = show tm1 ++ " := " ++ show tm2
    show (TmLoc l) = "location " ++ show l
    show (TmLet v tm1 tm2) = "let " ++ v ++ " = " ++ show tm1 ++ " in " ++ show tm2

data Ty = TyBool
        | TyNat
        | TyArr Ty Ty
        | TyUnit
        | TyRef Ty
          deriving Eq

instance Show Ty where
    show (TyBool) = "Bool"
    show (TyNat) = "Nat"
    show (TyArr ty1 ty2) = "(" ++ show ty1 ++ " -> " ++ show ty2 ++ ")"
    show (TyUnit) = "Unit"
    show (TyRef ty) = "(" ++ "Ref " ++ show ty ++ ")"