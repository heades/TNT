------------------------------------------------------------------------
-- This file implements several tests for RecCalc.                    --
--                                                                    --
-- Course: CSCI-3300 (Fall 2014)                                      --
-- Instructor: Prof. Eades                                            --
------------------------------------------------------------------------
module Tests where

import Parser
import Pretty
import TypeCheck
import Eval

typeTest :: String -> String -> Bool
typeTest termStr rStr = 
    let term = parseTerm termStr
        r    = runTypeChecker emptyCtx term in
        case r of
          Left a -> False 
          Right t -> t == (parseType rStr)

------------------------------------------------------------------------
-- Basic tests                                                        --
------------------------------------------------------------------------

cap = "(fun y : Nat -> Nat => (fun x : Nat => y)) (fun z : Nat => x)"

------------------------------------------------------------------------
-- Arithmetic                                                         --
------------------------------------------------------------------------

pre = "fun n : Nat => (rec n with 0 || (fun n : Nat => fun c : Nat => c))"
sub  = "fun n1 : Nat => fun n2 : Nat => (rec n2 with n1 || (fun n : Nat => fun c : Nat => app "++pre++" to n))"
plus = "fun n1 : Nat => fun n2 : Nat => (rec n1 with n2 || (fun n : Nat => fun c : Nat => suc n))"
mult = "fun n1 : Nat => fun n2 : Nat => (rec n1 with 0 || (fun n : Nat => fun c : Nat => app (app ("++plus++") to n2) to n))"
expi = "fun n1 : Nat => fun n2 : Nat => (rec n2 with (suc 0) || (fun n : Nat => fun c : Nat => app (app ("++mult++") to n1) to n))"

preTest = "app ("++pre++") to (suc (suc (suc (suc (suc 0)))))"
one = "app (app ("++sub++") to (suc (suc (suc 0)))) to (suc (suc 0))"
five = "app (app ("++plus++") to (suc (suc 0))) to (suc (suc (suc 0)))"
six = "app (app ("++mult++") to (suc (suc 0))) to (suc (suc (suc 0)))"
eight = "app (app ("++expi++") to (suc (suc 0))) to (suc (suc (suc 0)))"

ack = "fun m : Nat => fun n : Nat => (app (rec m with (fun y : Nat => (suc y)) || (fun h : Nat -> Nat => fun i : Nat => fun y : Nat => (rec y with (app h to (suc 0)) || (fun k : Nat => fun j : Nat => (app h to k))))) to n)"