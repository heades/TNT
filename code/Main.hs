------------------------------------------------------------------------
-- This file implements the main functions for typing checking,       --
-- evaluating, and testing type preservation of RecCalc programs.     --
--                                                                    --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Main where

import System.Console.Haskeline

import Parser
import Pretty
import TypeCheck
import Eval

------------------------------------------------------------------------
-- This function prompts the user for a typing context, a term        --
-- context, and a term, and then outputs the term's type with respect --
-- to the input contexts.                                             --
------------------------------------------------------------------------
mainCheck :: IO ()
mainCheck = do
  putStr "Context: "
  ctxStr <- getLine
  putStr "Term: "
  t <- getLine
  putStrLn $ let ctx  = parseCtx ctxStr
                 term = parseTerm t 
                 r    = runTypeChecker ctx term in
                 case r of
                   Left a -> a 
                   Right t -> runPrettyType t
------------------------------------------------------------------------
-- This function prompts the user for a term and then outputs its     --
-- normal form using the evaluator.                                   --
------------------------------------------------------------------------
mainEval :: IO ()
mainEval = do
  putStr "Term: "
  t <- getLine
  putStrLn.runPrettyTerm.runEval.parseTerm $ t
------------------------------------------------------------------------
-- mainPres prompts the user for a term and then tests that term for  --
-- type preservation.                                                 --
------------------------------------------------------------------------
mainPres :: IO ()
mainPres = do
  putStr "Term: "
  t <- getLine
  putStrLn.show.typePres.parseTerm $ t
