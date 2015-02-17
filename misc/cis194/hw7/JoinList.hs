module JoinList where

import Data.Monoid

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


