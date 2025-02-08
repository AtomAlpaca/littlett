module Equal where

import Base

alphaEqual :: Expr -> Expr -> Bool
alphaEqual e1 e2 = alphaEqualHelper 0 e1 [] e2 []

alphaEqualHelper :: Integer ->
              Expr -> [(Name, Integer)] ->
              Expr -> [(Name, Integer)] -> Bool
alphaEqualHelper i (Var x) xs (Var y) ys =
    case (lookup x xs, lookup y ys) of
    (Nothing, Nothing) -> x == y
    (Just a, Just b)   -> a == b
    (_, _)             -> False

alphaEqualHelper i (Lam x1 m1) xs1 (Lam x2 m2) xs2 =
    alphaEqualHelper (i + 1) m1 ((x1, i) : xs1) m2 ((x2, i) : xs2)

alphaEqualHelper i (App x1 y1) xs1 (App x2 y2) xs2 =
    alphaEqualHelper i x1 xs1 x2 xs2 &&
    alphaEqualHelper i y1 xs1 y2 xs2

alphaEqualHelper i (Pi x1 a1 b1) xs1 (Pi x2 a2 b2) xs2 =
    alphaEqualHelper i a1 xs1 a2 xs2 &&
    alphaEqualHelper (i + 1) b1 ((x1, i) : xs1) b2 ((x2, i) : xs2)

alphaEqualHelper i (Sigma x1 a1 b1) xs1 (Sigma x2 a2 b2) xs2 =
    alphaEqualHelper i a1 xs1 a2 xs2 &&
    alphaEqualHelper (i + 1) b1 ((x1, i) : xs1) b2 ((x2, i) : xs2)

alphaEqualHelper i (Cons x1 y1) xs1 (Cons x2 y2) xs2 =
    alphaEqualHelper i x1 xs1 x2 xs2 &&
    alphaEqualHelper i y1 xs1 y2 xs2

alphaEqualHelper i (Car x1) xs1 (Car x2) xs2 =
    alphaEqualHelper i x1 xs1 x2 xs2

alphaEqualHelper i (Cdr x1) xs1 (Cdr x2) xs2 =
    alphaEqualHelper i x1 xs1 x2 xs2

alphaEqualHelper _ Nat _ Nat _ = True

alphaEqualHelper _ Zero _ Zero _ = True

alphaEqualHelper i (Add1 x1) xs1 (Add1 x2) xs2 =
    alphaEqualHelper i x1 xs1 x2 xs2

alphaEqualHelper i (Rec t1 c1 b1 s1) xs1 (Rec t2 c2 b2 s2) xs2 =
    alphaEqualHelper i t1 xs1 t2 xs2 &&
    alphaEqualHelper i c1 xs1 c2 xs2 &&
    alphaEqualHelper i b1 xs1 b2 xs2 &&
    alphaEqualHelper i s1 xs1 s2 xs2

alphaEqualHelper _ Trustme _ Trustme _ = True
alphaEqualHelper _ _ _ _ _ = False
