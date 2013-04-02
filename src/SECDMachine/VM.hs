
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
            | Nil
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

-- load constant
exec (SECD s e ((LDC n):c) d) = Continue $ SECD ((Num n):s) e c d

-- arithmetic operators
exec (SECD ((Num y):(Num x):s) e (ADD:c) d) = Continue $ SECD ((Num (x + y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (SUB:c) d) = Continue $ SECD ((Num (x - y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (MUL:c) d) = Continue $ SECD ((Num (x * y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (DIV:c) d) = Continue $ SECD ((Num (x `div` y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (REM:c) d) = Continue $ SECD ((Num (x `rem` y)):s) e c d

-- operators for testing equality
exec (SECD ((Num y):(Num x):s) e (EQL:c) d) = Continue $ SECD ((Bool (x == y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (LEQ:c) d) = Continue $ SECD ((Bool (x <= y)):s) e c d

-- list operators
exec (SECD (a:b:s) e (CONS:c) d) = Continue $ SECD ((Cons a b):s) e c d
exec (SECD ((Cons a _):s) e (CAR:c) d) = Continue $ SECD (a:s) e c d
exec (SECD ((Cons _ b):s) e (CDR:c) d) = Continue $ SECD (b:s) e c d

-- push nil pointer
exec (SECD s e (NIL:c) d) = Continue $ SECD (Nil:s) e c d

-- exit evaluation
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

