module Base where

newtype Name = Name String
    deriving (Show, Eq)

data Expr =
      Var Name -- a
    | Lam Name Expr -- \x.M
    | App Expr Expr
    | Pi  Name Expr Expr
    | Sigma Name Expr Expr
    | Cons Expr Expr
    | Car  Expr
    | Cdr  Expr
    | Nat
    | Zero
    | Add1 Expr
    | Rec  Expr Expr Expr Expr
    | Trustme
    deriving Show
