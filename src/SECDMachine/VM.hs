
----------------------------------------------------------------------------
-- |
-- Module : SECDMachine.VM
--
-- Maintainer : jetho@gmx.de
-- Stability : unstable
-- Portability : portable
--
-- This module contains the Evaluator for the SECD Virtual Machine.
--
-----------------------------------------------------------------------------


module SECDMachine.VM where

import SECDMachine.Commands
import Control.Monad


data Value =  Num Int
            | Bool Bool
            | Cons Value Value
            | Closure Sequence Env
              deriving (Eq, Show)


type Stack = [Value]
type Env = [[Value]]
type Code = [Command]
type Dump = [(Stack, Env, Code)]

data SECD = SECD Stack Env Code Dump deriving Show

data Step a b =  Continue a
               | Finished b
               | Error String


exec :: SECD -> Step SECD Stack

exec (SECD s e ((LDC n):c) d) = Continue $ SECD ((Num n):s) e c d


exec (SECD s e (STOP:_) d) = Finished s

exec secd = Error $ "Invalid Machine State: " ++ (show secd)

run' (Continue secd) = run' (exec secd)
run' otherwise = otherwise 

run code = unwrap $ run' initSECD
    where initSECD = Continue $ SECD [] [] code []
          unwrap (Finished stack) = head' stack
          unwrap (Error msg) = Left msg
          head' (v:_) = Right v
          head' [] = Left "No valid result left on stack"

