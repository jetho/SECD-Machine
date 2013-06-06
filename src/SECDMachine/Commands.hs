
----------------------------------------------------------------------------
-- |
-- Module : SECDMachine.Commands
--
-- Maintainer : jetho@gmx.de
-- Stability : unstable
-- Portability : portable
--
-- This module specifies the core commands for the SECD machine.
--
-----------------------------------------------------------------------------


module SECDMachine.Commands where

type Level = Int
type Pos = Int
type Sequence = [Command]
type TrueSequence = Sequence
type FalseSequence = Sequence


data Command = ADD
             | SUB
             | MUL
             | DIV
             | REM
             | EQL
             | LEQ 
             | LDC Int
             | LD (Level, Pos)
             | STOP
             | NIL
             | JOIN
             | SEL TrueSequence FalseSequence
             | TSEL TrueSequence FalseSequence
             | LDF Sequence
             | AP
             | TAP
             | CONS
             | CAR
             | CDR
             | ATOM
             | RTN
             | DUM
             | RAP           
               deriving (Eq, Show)

