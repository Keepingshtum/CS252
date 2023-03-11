{-
  Name: Anant Shukla
  Class: CS 252
  Assigment: HW2
  Date: 3/10/23
  Description: The program is an interpreter for the WHILE semantics as defined in while-semantics file.
-}


module WhileInterp (
  Expression(..),
  Binop(..),
  Value(..),
  testProgram,
  run
) where

import Data.Map (Map)
import qualified Data.Map as Map

-- We represent variables as strings.
type Variable = String

-- The store is an associative map from variables to values.
-- (The store roughly corresponds with the heap in a language like Java).
type Store = Map Variable Value

data Expression =
    Var Variable                            -- x
  | Val Value                               -- v
  | Assign Variable Expression              -- x := e
  | Sequence Expression Expression          -- e1; e2
  | Op Binop Expression Expression
  | If Expression Expression Expression     -- if e1 then e2 else e3
  | While Expression Expression             -- while (e1) e2
  | AND Expression Expression               -- e1 AND e2
  | OR Expression Expression                -- e1 OR e2
  | NOT Expression                          -- NOT e
  deriving (Show)

data Binop =
    Plus     -- +  :: Int  -> Int  -> Int
  | Minus    -- -  :: Int  -> Int  -> Int
  | Times    -- *  :: Int  -> Int  -> Int
  | Divide   -- /  :: Int  -> Int  -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  -- | Andd     -- Andd :: Val -> Val -> Bool
  -- | Orr      -- Orr :: Val -> Val -> Bool
  -- | Nott     -- Nott :: Val -> Bool ... Not defining here because Not is not a Binary operator!
  deriving (Show)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Show,Eq)


-- This function will be useful for defining binary operations.
-- The first case is done for you.
-- Be sure to explicitly check for a divide by 0 and throw an error.
applyOp :: Binop -> Value -> Value -> Value
applyOp Plus (IntVal i) (IntVal j) = IntVal $ i + j
applyOp Minus (IntVal i) (IntVal j) = IntVal $ i - j
applyOp Times (IntVal i) (IntVal j) = IntVal $ i * j
applyOp Divide (IntVal i) (IntVal j)
  | j == 0 = error "Divide by zero error"
  | otherwise = IntVal $ i `div` j
applyOp Gt (IntVal i) (IntVal j) = BoolVal $ i > j
applyOp Ge (IntVal i) (IntVal j) = BoolVal $ i >= j
applyOp Lt (IntVal i) (IntVal j) = BoolVal $ i < j
applyOp Le (IntVal i) (IntVal j) = BoolVal $ i <= j
-- applyOp Andd (BoolVal i) (BoolVal j)
--   | i == False = BoolVal False
--   | otherwise = BoolVal $ i && j
-- applyOp Orr (BoolVal i) (BoolVal j)
--   | i == True = BoolVal True
--   | otherwise = BoolVal $ i || j
-- applyOp Nott (BoolVal i) = BoolVal $ not i
applyOp _ _ _ = error "Operation not supported!"


-- Implement this function according to the specified semantics
evaluate :: Expression -> Store -> (Value, Store)
evaluate (Op o e1 e2) s =
  let (v1,s1) = evaluate e1 s
      (v2,s') = evaluate e2 s1
  in (applyOp o v1 v2, s')

-- Retrieving from Storage
evaluate (Var x) s =
  case Map.lookup x s of
    Just v -> (v, s)
    Nothing -> error ("Variable " ++ x ++ " not found")

-- Adding to Storage
evaluate (Val v) s = (v, s)

-- Applying := 
evaluate (Assign x e) s =
  let (v, s') = evaluate e s
  in (v, Map.insert x v s')

-- Execute the statements that are chained by ;
evaluate (Sequence e1 e2) s =
  let (_, s1) = evaluate e1 s
  in evaluate e2 s1

-- Handle if conditions
evaluate (If e1 e2 e3) s =
  let (BoolVal b, s1) = evaluate e1 s
  in if b then evaluate e2 s1 else evaluate e3 s1

-- Handle while conditions
evaluate (While e1 e2) s =
  let (BoolVal b, s1) = evaluate e1 s
  in if b then evaluate (While e1 e2) (snd (evaluate e2 s1)) else (BoolVal False, s1)

-- Handle AND
evaluate (AND e1 e2) s 
  | exp1 == (BoolVal False) = (BoolVal False, s)
  | otherwise = evaluate e2 s1
  where (exp1, s1) = evaluate e1 s

-- Handle OR
evaluate (OR e1 e2) s 
  | exp1 == (BoolVal True) = (BoolVal True,s)
  | otherwise = evaluate e2 s1
  where (exp1,s1) = evaluate e1 s

-- Handle NOT
evaluate (NOT e1) s 
  | exp1 == (BoolVal False) = (BoolVal True,s)
  | otherwise = (BoolVal False,s)
  where (exp1,s1) = evaluate e1 s




-- In case any other pattern is encountered
-- evaluate _ _ = error "Evaluation not implemented"


-- Evaluates a program with an initially empty state
run :: Expression -> (Value, Store)
run prog = evaluate prog Map.empty

-- The same as run, but only returns the Store
testProgram :: Expression -> Store
testProgram prog = snd $ run prog


