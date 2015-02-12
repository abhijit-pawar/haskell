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

--Excercise 03
--The TypeClass
class Expr a where
	lit :: Integer -> a	 
	add :: a -> a -> a
	mul :: a -> a -> a

--make the ExprT an instance of Expr
instance Expr ExprT where 
	lit = Lit
	add = Add
	mul = Mul

instance Expr Integer where
	lit = id
	add = (+)
	mul = (*)

reify :: ExprT -> ExprT
reify = id


