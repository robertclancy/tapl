-- Haskell implementation of Arithmetic Expressions
module Arith where
    data Term =
        Tru |
        Fls |
        If Term Term Term |
        Zero |
        Succ Term |
        Pred Term |
        IsZero Term
        deriving Show

    isNumericalVal :: Term -> Bool
    isNumericalVal Zero     = True
    isNumericalVal (Succ _) = True
    isNumericalVal _        = False

    isVal :: Term -> Bool
    isVal Tru = True
    isVal Fls = True
    isVal t = isNumericalVal t

    -- Evaluate a term using big-step style
    eval :: Term -> Term
    eval Tru = Tru
    eval Fls = Fls
    eval Zero = Zero
    eval (If t x y) = evalIf (eval t) x y
                    where evalIf Tru b _ = eval b
                          evalIf Fls _ b = eval b
                          evalIf _   _ _ = error "If applied to non-numeric val."
    eval (Succ t) = evalSucc (eval t)
                  where evalSucc nv | isNumericalVal nv = Succ nv
                                    | otherwise         = error "Succ applied to non-numeric val."
    eval (Pred t) = evalPred (eval t)
                  where evalPred Zero = Zero
                        evalPred (Succ nv) | isNumericalVal nv = nv
                                           | otherwise         = error "Pred applied to non-mumeric val."
                        evalPred _    = error "Pred does not match."
    eval (IsZero t) = evalIsZero (eval t)
                    where evalIsZero Zero = Tru
                          evalIsZero (Succ nv) | isNumericalVal nv = Fls
                                               | otherwise         = error "IsZero applied to non-numeric val."
                          evalIsZero _    = error "IsZero does not match."
