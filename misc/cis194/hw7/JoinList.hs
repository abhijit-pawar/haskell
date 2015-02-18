{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Editor
import Buffer

data JoinList m a = Empty 
			| Single m a 
			| Append m (JoinList m a) (JoinList m a)
		deriving (Eq, Show)

--Excerccise 01
{-- Write an append function for JoinLists that yields a new JoinList whose monoidal
	annotation is derived from those of the two arguments
--}
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2  = Append (tag jl1 <> tag jl2 ) jl1 jl2

--Helper function to get the root of the JoinList. 
--As tag is a function working on typeclass for Monoid of JoinList, we have to implement all the data constructors for JoinList for a Monoid

tag :: Monoid m => JoinList m a -> m
tag Empty 		= mempty
tag (Single m a) 	= m
tag (Append m _ _ ) 	= m


--Excercise 02
--1. Implment the indexing

--get the size of the JoinList
sizeOf :: (Sized m , Monoid m) => JoinList m a -> Int
sizeOf = getSize.size.tag

indexJ :: (Sized b , Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i jl | i > sizeOf jl = Nothing
indexJ i (Single _ a) = Just a
indexJ i (Append m l r ) | i < sizeOf l = indexJ i l
			 | otherwise = indexJ (i - sizeOf l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a 
dropJ _ Empty = Empty
dropJ i a | i <= 0 = a
dropJ i (Single m a) | i == 1 = Empty
dropJ i jl | i > sizeOf jl = Empty
dropJ i (Append m l r ) 
	| i < sizeOf l = (dropJ i l) +++ r
	| otherwise = dropJ ( i - sizeOf l) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i a | i <= 0 = a
takeJ i jl | i > sizeOf jl = jl
takeJ i (Append m l r )
        | i < sizeOf l = takeJ i l 
        | otherwise = l +++ takeJ ( i - sizeOf l) r
