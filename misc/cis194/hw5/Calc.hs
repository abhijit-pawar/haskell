module Calc where

import ExprT as ET

--Excercise 01

eval :: ExprT -> Integer
eval (Lit x) = x
eval (ET.Add x y) = (eval x) + (eval y)
eval (ET.Mul x y) = (eval x) *( eval y)
