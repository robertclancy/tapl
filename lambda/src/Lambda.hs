{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import Data.List (elemIndex, (\\), genericIndex)
import Data.Set (Set, toList, delete, union, singleton)
import Control.Arrow

data NamedTerm = NVar String | NAbs String NamedTerm | NApp NamedTerm NamedTerm
               deriving Show

data Term = Var Integer | Abs Term | App Term Term
          deriving Show

class Scope s a where
        find :: a -> s -> Maybe Integer
        push :: a -> s -> s
        pop :: s -> (a, s)
        findAt :: Integer -> s -> a

newtype NamingContext a = NamingContext [a]
                          deriving Show

instance (Eq a) => Scope (NamingContext a) a where
        find x (NamingContext ctx) = fmap toInteger $ elemIndex x ctx
        push x (NamingContext ctx) = NamingContext (x : ctx)
        pop (NamingContext ctx) = (head ctx, NamingContext $ tail ctx)
        findAt x (NamingContext ctx) = genericIndex ctx x

type Context = NamingContext String

freeVariables :: NamedTerm -> NamingContext String
freeVariables = NamingContext . toList . freeVars
                where freeVars :: NamedTerm -> Set String
                      freeVars (NVar x) = singleton x
                      freeVars (NAbs x t) = Data.Set.delete x $ freeVars t
                      freeVars (NApp s t) = Data.Set.union (freeVars s) (freeVars t)

unnameTerm :: NamedTerm -> (Context, Term)
unnameTerm nt = (ctx, term)
                where ctx = freeVariables nt
                      term = uTerm ctx nt
                      uTerm :: Context -> NamedTerm -> Term
                      uTerm c (NVar x) = case find x c of
                                             Just index -> Var index
                                             Nothing -> error $ "Could not find variable " ++ x ++ " in scope"
                      uTerm c (NAbs x t) = Abs $ uTerm (push x c) t
                      uTerm c (NApp s t) = App (uTerm c s) (uTerm c t)

defaultContext :: Context
defaultContext = NamingContext vars
                 where vars = let numbers = [1..] :: [Integer]
                                  snumbers = "" : map show numbers
                                  letters = enumFromTo 'a' 'z' in
                                      do
                                          number <- snumbers
                                          letter <- letters
                                          return $ [letter] ++ number

diff :: Context -> Context -> Context
diff (NamingContext xs) (NamingContext ys) = NamingContext $ ys \\ xs

nameTerm :: Context -> Term -> NamedTerm
nameTerm ctx t = nameTerm' ctx (diff ctx defaultContext) t
                 where nameTerm' :: Context -> Context -> Term -> NamedTerm
                       nameTerm' c _ (Var x) = NVar (findAt x c)
                       nameTerm' c nextVar (Abs s) = NAbs x (nameTerm' ctx' nextVar' s)
                                                       where (x, nextVar') = pop nextVar
                                                             ctx' = push x c
                       nameTerm' c nextVar (App s u) = NApp (nameTerm' c nextVar s) (nameTerm' c nextVar u)


evalNamedTerm :: NamedTerm -> NamedTerm
evalNamedTerm = (uncurry nameTerm) . (second eval) . unnameTerm

eval :: Term -> Term
eval (Var x) = Var x
eval (Abs t) = Abs t
eval (App (Var s) t) = App (Var s) $ eval t
eval (App (Abs t) s) = eval $ shift (-1) $ substitute 0 (shift 1 (eval s)) t
eval (App (App u v) s) = case eval $ App u v of
                             App x y -> App (App x y) $ eval s
                             t -> eval $ App t s

substitute :: Integer -> Term -> Term -> Term
substitute k v (Var x) = if k == x then v else Var x
substitute k v (Abs t) = Abs $ substitute (k + 1) (shift 1 v) t
substitute k v (App a b) = App (substitute k v a) (substitute k v b)

shift :: Integer -> Term -> Term
shift m t = shift' m 0 t
            where shift' :: Integer -> Integer -> Term -> Term
                  shift' n c (Var x) = if x < c then Var x else Var $ x + n
                  shift' n c (App s u) = App (shift' n c s) (shift' n c u)
                  shift' n c (Abs u) = Abs (shift' n (succ c) u)
