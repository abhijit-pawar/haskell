{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT as ET
import Parser
import StackVM as SVM


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
	add = ET.Add
	mul = ET.Mul

reify :: ExprT -> ExprT
reify = id

--Excercise 04

instance Expr Integer where
        lit = id
        add = (+)
        mul = (*)

instance Expr Bool where
	lit a  
		| a > 0 = True
		| a <=0 = False
	add = (||)
	mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq,Show)

instance Expr MinMax where
	lit = MinMax
	add (MinMax x) (MinMax y) = MinMax (max x y)
	mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Show,Eq)

instance Expr Mod7 where
	lit x = Mod7 ( x `mod` 7)
	add (Mod7 x) (Mod7 y) = Mod7 ( (x + y) `mod` 7)
	mul (Mod7 x) (Mod7 y) = Mod7 ( (x*y) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3* -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

--Excercise 05
--create an instance of Expr for Program
instance Expr Program where
	lit a   = [PushI a]
	add a b = a ++ b ++ [SVM.Add]
	mul a b = a ++ b ++ [SVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul 

