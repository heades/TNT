------------------------------------------------------------------------
-- This file implements the type construction algorithm for RecCalc. --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module TypeCheck (module Control.Monad.Trans.Error,
                  module Syntax,
                  Ctx,
                  emptyCtx,
                  extCtx,
                  typeCheck,
                  runTypeChecker,
                  testTypeCheckCLTerm
) where

import Control.Monad.Trans.Error    

import Syntax
import Parser
import Pretty 

------------------------------------------------------------------------
-- Typing Contexts.                                                   --
------------------------------------------------------------------------
type Ctx = [(TmName, Type)]

------------------------------------------------------------------------
-- The empty context.                                                 --
------------------------------------------------------------------------
emptyCtx :: Ctx
emptyCtx = []

-------------------------------------------------------------------------
-- Extension functions for contexts.  This simply adds a term name     --
-- and a corresponding type to the context.                            --
-------------------------------------------------------------------------
extCtx :: Ctx -> TmName -> Type -> Ctx
extCtx ctx nm ty = (nm, ty) : ctx

-------------------------------------------------------------------------
-- This is the type construction function.  Given a context and a term --
-- this function will either return the type the input term inhabits   --
-- or throws an error.                                                 --
-------------------------------------------------------------------------
typeCheck :: Fresh m => Ctx -> Term -> ErrorT String m Type

typeCheck ctx (Var nm) = 
    case lookup nm ctx of
      Just ty -> return ty
      _ -> throwError ("Error: "++n2s nm++" was not found in the context.")

typeCheck ctx (Fun ty b) = do
  (x,t) <- unbind b
  ty' <- typeCheck (extCtx ctx x ty) t
  return $ Arr ty ty'

typeCheck ctx (App t1 t2) = do
  ty <- typeCheck ctx t1
  ty' <- typeCheck ctx t2
  case ty of 
    Arr ty1 ty2 | ty1 == ty' -> return ty2
    _ -> throwError $ "Type error in App"

typeCheck ctx Zero = return Nat

typeCheck ctx (Suc t) = do
  ty <- typeCheck ctx t
  case ty of
    Nat -> return ty
    _ -> throwError $ "Type error in Suc"

typeCheck ctx (Rec t t1 t2) = do
  ty <- typeCheck ctx t
  case ty of
    Nat -> do
            ty1 <- typeCheck ctx t1
            ty2 <- typeCheck ctx t2
            case ty2 of
              Arr ty3 (Arr Nat ty4) -> 
                  case ((ty1 == ty3) &&  (ty3 == ty4)) of
                    True -> return ty1
                    False -> throwError $ "Type error in Rec: types are not equal."
              _ -> throwError $ "Type error in Rec: wrong type in base case."
    _ -> throwError $ "Type error in Rec: counter must be a Nat."

runTypeChecker :: Ctx -> Term -> Either String Type
runTypeChecker ctx term = runFreshM.runErrorT $ typeCheck ctx term

testTypeCheckCLTerm :: String -> String
testTypeCheckCLTerm s =
    let t = parseTerm s
        r = runTypeChecker [] t
     in case r of
          Left s -> s
          Right t -> runPrettyType t 
       