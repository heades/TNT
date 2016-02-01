------------------------------------------------------------------------
-- This file implements the AdjSep3 pretty printers.                  --
------------------------------------------------------------------------
module Pretty where

import Data.List
import Data.Char
import Text.Parsec

import Syntax
--import Parser 

------------------------------------------------------------------------
-- Type pretty printers                                               --
------------------------------------------------------------------------

prettyLType :: Fresh m => LType -> m String
prettyLType TRE = return "True"
                 
prettyLType (Imp ty1 ty2) = do
  s1 <- prettyLType ty1
  s2 <- prettyLType ty2
  return $ (if isInfixOf "->" s1
            then "("++s1++")"
            else s1) ++ " -> "++s2

prettyLType (And ty1 ty2) = do
  s1 <- prettyLType ty1
  s2 <- prettyLType ty2
  return $ (if isInfixOf "*" s1
            then "("++s1 ++ ")"
            else s1) ++
           " * "
           ++
           (if isInfixOf "*" s2
            then "("++s2 ++ ")"
            else s2)

prettyLType (D ty) = do
  s <- prettyPType ty
  return $ "D("++s++")"

prettyPType :: Fresh m => PType -> m String
prettyPType Unit = return "1"
prettyPType Nat = return "Nat"
                  
prettyPType (Arw ty1 ty2) = do
  s1 <- prettyPType ty1
  s2 <- prettyPType ty2
  return $ (if isInfixOf "->" s1
            then "("++s1++")"
            else s1) ++ " -> "++s2

prettyPType (Prod ty1 ty2) = do
  s1 <- prettyPType ty1
  s2 <- prettyPType ty2
  return $ (if isInfixOf "*" s1
            then "("++s1 ++ ")"
            else s1) ++
           " * "
           ++
           (if isInfixOf "*" s2
            then "("++s2 ++ ")"
            else s2)

prettyPType (T ty) = do
  s <- prettyLType ty
  return $ "T("++s++")"       

------------------------------------------------------------------------
-- Term pretty printers                                               --
------------------------------------------------------------------------
prettyLTerm :: Fresh m => LTerm -> m String

prettyLTerm (LVar nm) = return $ n2s nm

prettyLTerm (LFun ty b) = do
  tyS <- prettyLType ty
  (x, t) <- unbind b
  ts <- prettyLTerm t
  return $ "fun "++n2s x++" : "++tyS++" => "++ts
         
prettyLTerm (LApp t1 t2) = do
  t1s <- prettyLTerm t1
  t2s <- prettyLTerm t2
  return $ "app "++t1s++" to "++t2s

prettyLTerm (D' t) = do
  ts <- prettyPTerm t
  return $ "D("++ts++")"

prettyLTerm (Let b t) = do
  s <- prettyLTerm t
  (x, t') <- unbind b
  s' <- prettyLTerm t'
  return $ "let "++n2s x++" = "++s'++" in "++s

prettyLTerm (Derelict t) = do
  ts <- prettyPTerm t
  return $ "derelict ("++ts++")"

prettyPTerm :: Fresh m => PTerm -> m String

prettyPTerm (PVar nm) = return $ n2s nm

prettyPTerm (PFun ty b) = do
  tyS <- prettyPType ty
  (x, t) <- unbind b
  ts <- prettyPTerm t
  return $ "fun "++n2s x++" : "++tyS++" => "++ts
         
prettyPTerm (PApp t1 t2) = do
  t1s <- prettyPTerm t1
  t2s <- prettyPTerm t2
  return $ "app "++t1s++" to "++t2s

prettyPTerm (T' t) = do
  ts <- prettyLTerm t
  return $ "T("++ts++")"
         
testPretty pretty s = do
  runFreshM (pretty s)

-- testPretty parser pretty s = do
--   let o = parse parser "" s in  
--     case o of
--       Left e -> error $ show e
--       Right r -> runFreshM (pretty r)
