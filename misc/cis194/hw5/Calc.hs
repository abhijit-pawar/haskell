module Calc where

import ExprT as ET
import Parser

--Excercise 01

eval :: ExprT -> Integer
eval (Lit x) = x
eval (ET.Add x y) = (eval x) + (eval y)
eval (ET.Mul x y) = (eval x) *( eval y)

--Excercise 02
evalStr :: String -> Maybe Integer
evalStr p = case parseExp Lit ET.Add ET.Mul p of
		Nothing -> Nothing
		Just e  -> Just (eval e)
 
evalStrFunctor :: String -> Maybe Integer
evalStrFunctor = fmap eval . parseExp Lit ET.Add ET.Mul
