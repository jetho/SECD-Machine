
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
import Data.Maybe


type ErrorMsg = String

data Value =  Num Int
            | Bool Bool
            | Cons Value Value
            | Closure Sequence Env
            | Nil
              deriving (Eq, Show)


-- the structure of the SECD machine
type Stack = [Value]
type Env = [[Value]]
type Code = [Command]
type Dump = [(Stack, Env, Code)]

data SECD = SECD Stack Env Code Dump deriving Show

-- data types for execution control
data Step a b =  Continue a
               | Finished b
               | Error String


-- helper functions

-- locate a binding in the environment
locate :: Level -> Pos -> Env -> Maybe Value
locate lev pos = nth lev >=> nth pos
    where nth n = listToMaybe . drop n

-- convert a cons structure to a list
cons2list Nil = []
cons2list (Cons a b) = a : cons2list b

-- extending an environment for function application
extendEnv env args = (cons2list args) : env

-- select branch of conditional
select True = const
select False = flip const


-- execute one instruction
exec :: SECD -> Step SECD Stack

-- load constant
exec (SECD s e ((LDC n):c) d) = Continue $ SECD ((Num n):s) e c d

-- load function
exec (SECD s e ((LDF c'):c) d) = Continue $ SECD ((Closure c' e):s) e c d

-- load variable binding
exec secd@(SECD s e (ld@(LD (i, j)):c) d) =
    maybe (Error $ "No valid binding found for '" ++ (show ld) ++ "' in " ++ (show secd))
          (\v -> Continue $ SECD (v:s) e c d)
          (locate i j e)

-- arithmetic operators
exec (SECD ((Num y):(Num x):s) e (ADD:c) d) = Continue $ SECD ((Num (x + y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (SUB:c) d) = Continue $ SECD ((Num (x - y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (MUL:c) d) = Continue $ SECD ((Num (x * y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (DIV:c) d) = Continue $ SECD ((Num (x `div` y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (REM:c) d) = Continue $ SECD ((Num (x `rem` y)):s) e c d

-- operators for equality tests
exec (SECD ((Num y):(Num x):s) e (EQL:c) d) = Continue $ SECD ((Bool (x == y)):s) e c d
exec (SECD ((Num y):(Num x):s) e (LEQ:c) d) = Continue $ SECD ((Bool (x <= y)):s) e c d

-- list operators
exec (SECD (a:b:s) e (CONS:c) d) = Continue $ SECD ((Cons a b):s) e c d
exec (SECD ((Cons a _):s) e (CAR:c) d) = Continue $ SECD (a:s) e c d
exec (SECD ((Cons _ b):s) e (CDR:c) d) = Continue $ SECD (b:s) e c d

-- push nil pointer
exec (SECD s e (NIL:c) d) = Continue $ SECD (Nil:s) e c d

-- conditional operators
-- tail recursive "if"
exec (SECD ((Bool b):s) e ((TSEL tc fc):c) d) = Continue $ SECD s e (select b tc fc) d
-- non-tail recursive
exec (SECD ((Bool b):s) e ((SEL tc fc):c) d) = Continue $ SECD s e (select b tc fc) (([],[],c):d)
exec (SECD s e [JOIN] (([],[],c):d)) = Continue $ SECD s e c d

-- test for atomic value
exec (SECD (a:s) e (ATOM:c) d) = Continue $ SECD ((Bool (atomic a)):s) e c d
    where atomic (Num _) = True
          atomic (Bool _) = True
          atomic _ = False

-- function application (let)
-- optimize tail calls
exec (SECD ((Closure c' e'):args:s) e (TAP:_) d) = Continue $ SECD s (extendEnv e' args) c' d
-- non-tail calls
exec (SECD ((Closure c' e'):args:s) e (AP:c) d) = Continue $ SECD [] (extendEnv e' args) c' ((s,e,c):d)

-- recursive function application (letrec)
exec (SECD s e (DUM:c) d) = Continue $ SECD s ([]:e) c d
exec (SECD ((Closure c' e'):bs:s) ([]:e) (RAP:c) d) = Continue $ SECD [] (recBindings:e) c' ((s,e,c):d)
    where recBindings = map bind (cons2list bs)
          bind (Closure c ([]:e)) = Closure c (recBindings:e)
          bind x = x

-- return from function and leave result on top of stack
exec (SECD (res:_) e' (RTN:_) ((s,e,c):d)) = Continue $ SECD (res:s) e c d

-- exit evaluation
exec (SECD s e (STOP:_) d) = Finished s

exec secd = Error $ "Invalid Machine State: " ++ (show secd)


run' (Continue secd) = run' (exec secd)
run' otherwise = otherwise

run :: Code -> Either ErrorMsg Value
run code = unwrap $ run' initSECD
    where initSECD = Continue $ SECD [] [] code []
          unwrap (Finished stack) = head' stack
          unwrap (Error msg) = Left msg
          head' = maybe (Left "No valid result left on stack") Right . listToMaybe

