------------------------------------------------------------------------
-- This file implements the evaluation algorithm for RecCalc          --
-- programs.                                                          --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Eval where

import Syntax
import Parser
import TypeCheck
import Pretty

------------------------------------------------------------------------
-- The evaluator.                                                     --
------------------------------------------------------------------------
eval :: Fresh m => Term -> m Term

eval (Var nm) = return $ Var nm

eval Zero = return Zero

eval (Fun ty b) = do
  (x, t) <- unbind b
  n <- eval t
  return $ Fun ty $ bind x n

eval (App t1 t2) = do
  n1 <- eval t1
  n2 <- eval t2
  case n1 of
    Fun ty b -> 
        do (x, n'1) <- unbind b
           eval $ replace x n2 n'1
    _ -> return $ App n1 n2

eval (Suc t) = do
  n <- eval t
  return $ Suc n

eval (Rec t t1 t2) = do
  n <- eval t
  n1 <- eval t1
  case n of
    Zero -> return n1
    (Suc n') -> do n2 <- eval t2
                   eval $ App (App n2 (Rec n' n1 n2)) n'
    _ -> do n2 <- eval t2
            return $ Rec n n1 n2

------------------------------------------------------------------------
-- This function makes it easy to run the evaluator.                  --
------------------------------------------------------------------------
runEval :: Term -> Term
runEval = runFreshM.eval

testEval :: String -> String
testEval = runPrettyTerm.runEval.parseTerm

------------------------------------------------------------------------
-- This function tests a RecCalc program for type preservation.      --
------------------------------------------------------------------------
typePres :: Term -> Bool
typePres t = 
    let tty = runFreshM.runErrorT $ typeCheck emptyCtx t 
        n = runFreshM $ eval t
        nty = runFreshM.runErrorT $ typeCheck emptyCtx n in
        tty == nty
