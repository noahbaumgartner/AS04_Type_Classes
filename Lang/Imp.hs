{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}

module Lang.Imp where

import qualified Map.TreeMap as M

todo :: a
todo = error "TODO"

type Name = String

type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
  BNot :: Expr Bool -> Expr Bool
  BAnd :: Expr Bool -> Expr Bool -> Expr Bool
  BOr :: Expr Bool -> Expr Bool -> Expr Bool
  REq :: Expr Integer -> Expr Integer -> Expr Bool
  RLt :: Expr Integer -> Expr Integer -> Expr Bool
  AConst :: Integer -> Expr Integer
  AVar :: Name -> Expr Integer
  APlus :: Expr Integer -> Expr Integer -> Expr Integer
  AMinus :: Expr Integer -> Expr Integer -> Expr Integer
  AMul :: Expr Integer -> Expr Integer -> Expr Integer
  ADiv :: Expr Integer -> Expr Integer -> Expr Integer
  AMod :: Expr Integer -> Expr Integer -> Expr Integer

deriving instance Eq (Expr a)

deriving instance Show (Expr a)

data Cmd
  = CSkip
  | CAssign Name (Expr Integer)
  | CIfThEl (Expr Bool) Cmd Cmd
  | CWhile (Expr Bool) Cmd
  | CSeq [Cmd]

eval :: Expr a -> State -> a
eval (BNot b) st = not (eval b st)
eval (BAnd b1 b2) st = eval b1 st && eval b2 st
eval (BOr b1 b2) st = eval b1 st || eval b2 st
eval (REq a1 a2) st = eval a1 st == eval a2 st
eval (RLt a1 a2) st = eval a1 st < eval a2 st
eval (AConst n) _ = n
eval (AVar name) st =
  case M.lookup name st of
    Just n -> n
    Nothing -> error ("Undefined variable: " ++ name)
eval (APlus a1 a2) st = eval a1 st + eval a2 st
eval (AMinus a1 a2) st = eval a1 st - eval a2 st
eval (AMul a1 a2) st = eval a1 st * eval a2 st
eval (ADiv a1 a2) st = eval a1 st `div` eval a2 st
eval (AMod a1 a2) st = eval a1 st `mod` eval a2 st

exec :: Cmd -> State -> State
exec CSkip st = st
exec (CAssign name expr) st = M.insert name (eval expr st) st
exec (CIfThEl cond thenCmd elseCmd) st =
  if eval cond st then exec thenCmd st else exec elseCmd st
exec (CWhile cond body) st
  | eval cond st = exec (CWhile cond body) (exec body st)
  | otherwise = st
exec (CSeq cmds) st = foldl (\currentState cmd -> exec cmd currentState) st cmds
