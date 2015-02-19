{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List


-- Excercise add an Employee to a GuestList
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL empss funs) = GL (emp:empss) (funs + empFun emp)

--Add Monoidal instance of the GuestList

instance Monoid GuestList where
	mempty = GL [] 0
	(GL es fun ) `mappend` (GL es' fun') = GL (es ++ es') (fun + fun')

--GuestList supports comparator on fun 	quotient as its instance of order
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max
