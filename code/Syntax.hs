{-# LANGUAGE NoMonomorphismRestriction, TemplateHaskell, 
             DeriveDataTypeable,        FlexibleInstances, 
             MultiParamTypeClasses,     FlexibleContexts, 
                                        UndecidableInstances #-} 
------------------------------------------------------------------------
-- This file contains the syntax and all syntax manipulation tools    --
-- for AdjSep3.                                                       --
------------------------------------------------------------------------

module Syntax (module Unbound.LocallyNameless, 
               module Unbound.LocallyNameless.Alpha,
               LTmName,
               PTmName,
               LType(And, TRE, Imp, D),
               PType(Nat, Prod, Unit, Arw, T),
               LTerm(LVar, LFun, LApp, D', Derelict, Let),
               PTerm(PVar, PFun, PApp, T'),
               n2s
              ) where

import Prelude
import Data.List
import Unbound.LocallyNameless 
import Unbound.LocallyNameless.Alpha

------------------------------------------------------------------------
-- The syntax for AdjSep3                                             -- 
------------------------------------------------------------------------
data LType =
    TRE                              -- True
  | And LType LType                  -- Conjunction 
  | Imp LType LType                  -- Implication
  | D PType                          -- Diverage Functor
  deriving (Show,Eq)

data PType =
    Nat                              -- Nat
  | Unit                             -- Unit
  | Prod PType PType                 -- Products
  | Arw PType PType                  -- Function types
  | T LType                          -- Termination Functor
  deriving (Show,Eq)  

type LTmName = Name LTerm            -- Logical Term names
type PTmName = Name PTerm            -- Programmatic Term names    
data LTerm =
    LVar LTmName                     -- Term variable
  | LFun LType (Bind LTmName LTerm)  -- Lambda abstraction
  | LApp LTerm LTerm                 -- Term application
  | D' PTerm                         -- Diverage Functor
  | Derelict PTerm                   -- T-inverse
  | Let (Bind LTmName LTerm) LTerm   -- D-Elim
  deriving (Show)

data PTerm =
    PVar PTmName                     -- Term variable
  | PFun PType (Bind PTmName PTerm)  -- Lambda abstraction
  | PApp PTerm PTerm                 -- Term application
  | T' LTerm                         -- Termination Functor
  deriving (Show)  

------------------------------------------------------------------------
-- This derives a bunch of machinery we can use for binding.          --
------------------------------------------------------------------------
$(derive [''LTerm,''LType,''PType, ''PTerm])
instance Alpha LTerm
instance Alpha PTerm    
instance Alpha LType
instance Alpha PType

instance Subst LTerm LType
instance Subst LTerm PType
instance Subst PTerm PType
instance Subst PTerm LType        
instance Subst LTerm LTerm where
  isvar (LVar x) = Just (SubstName x)
  isvar _ = Nothing
instance Subst LTerm PTerm
instance Subst PTerm LTerm where
instance Subst PTerm PTerm where
  isvar (PVar x) = Just (SubstName x)
  isvar _ = Nothing
            
------------------------------------------------------------------------
-- The variable substitution functions.                               --
------------------------------------------------------------------------
substTest :: Subst b a => Fresh m => Name b -> b -> a -> m a
substTest n t t' = return $ subst n t t'

------------------------------------------------------------------------
-- Some helpful redefinitions of Unbound functions.                   --
------------------------------------------------------------------------
n2s :: Name a -> String
n2s = name2String