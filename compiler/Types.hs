module Types where

type Iden = String
type Bloc = [Comm]
data Type = Doubletype | Inttype | Stringtype | Voidtype deriving Show
data Cons = Doublecons Double | Intcons Integer deriving Show
data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Neg Expr | Con Cons | Var Iden | Cal Iden [Expr] | Lit String deriving Show
data Rela = Expr :==: Expr | Expr :/=: Expr | Expr :<: Expr | Expr :>: Expr | Expr :<=: Expr | Expr :>=: Expr deriving Show
data Logi = Logi :&: Logi | Logi :|: Logi | Not Logi | Rel Rela deriving Show
data Vari = Iden :#: Type deriving Show
data Func = Iden :->: ([Vari], Type) deriving Show
data Comm = If Logi Bloc Bloc | While Logi Bloc | Attribution Iden Expr | Read Iden | Print Expr | Return (Maybe Expr) | Call Iden [Expr] deriving Show
data Prog = Program [Func][(Iden, [Vari], Bloc)][Vari] Bloc deriving Show