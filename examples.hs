
module Main where

import SECDMachine.Commands
import SECDMachine.VM


{-- recursive fact
letrec
    X = 5
    FACT = lambda (Y) if eq (Y, 0) then 1 else Y*FACT(Y-1)
in
    FACT(X)
end
--}
fact_ex = [DUM,NIL,LDF [LDC 0,LD (0,0),EQL,SEL [LDC 1,JOIN] [NIL,LD (0,0),LDC 1,SUB,CONS,LD (1,1),AP,LD (0,0),MUL,JOIN],RTN],CONS] ++
          [LDC 5,CONS,LDF [NIL,LD (0,0),CONS,LD (0,1),AP,RTN],RAP,STOP]



{-- nested let
let
   X = 5
in
  letrec
      ID = lambda (X) X
  in
      X
  end
end
--}
nested_ex = [NIL,LDC 5,CONS,LDF [DUM,NIL,LDF [LD (0,0),RTN],CONS,LDF [LD (1,0),RTN],RAP,RTN],AP,STOP]



{-- mutual recursion even <-> odd
letrec
    ODD  = lambda(X) if eq (X, 0) then false else EVEN(X-1)
    EVEN = lambda(X) if eq (X, 0) then true  else ODD(X-1)
in
    EVEN(6)
end
--}
even_ex = [DUM, NIL] ++
          [LDF [LD (0,0), LDC 0, EQL, SEL [LDC 1, LDC 1, EQL, JOIN] [NIL, LD (0,0), LDC 1, SUB, CONS, LD (1,0), AP, JOIN], RTN], CONS] ++
          [LDF [LD (0,0), LDC 0, EQL, SEL [LDC 1, LDC 0, EQL, JOIN] [NIL, LD (0,0), LDC 1, SUB, CONS, LD (1,1), AP, JOIN], RTN], CONS] ++
          [LDF [NIL, LDC 0, CONS, LD (0,1), AP, RTN], RAP, STOP]

