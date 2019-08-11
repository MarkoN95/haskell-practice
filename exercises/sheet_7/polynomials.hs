module Polynomials where

type Poly a = [(Integer, [a])]

data SymbExpr a
  = Var a
  | Lit Integer
  | Add (SymbExpr a) (SymbExpr a)
  | Mul (SymbExpr a) (SymbExpr a)
  deriving Show

evalPoly :: (a -> Integer) -> Poly a -> Integer
evalPoly eval poly = foldl (\acc (c, vars) -> acc + c * (foldl (*) 1 (map eval vars))) 0 poly

foldSymbExpr :: (a -> b) -> (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> SymbExpr a -> b
foldSymbExpr v l a m (Var var) = v var
foldSymbExpr v l a m (Lit i) = l i
foldSymbExpr v l a m (Add lhs rhs) = a (foldSymbExpr v l a m lhs) (foldSymbExpr v l a m rhs)
foldSymbExpr v l a m (Mul lhs rhs) = m (foldSymbExpr v l a m lhs) (foldSymbExpr v l a m rhs)

toPoly :: SymbExpr a -> Poly a
toPoly = foldSymbExpr (\v -> [(1, [v])])
                      (\l -> [(l, [])])
                      (++)
                      (\l r -> [ (lc * rc, lvars ++ rvars)  | (lc, lvars) <- l, (rc, rvars) <- r])
