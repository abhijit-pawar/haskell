{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List


-- Excercise add an Employee to a GuestList
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL empss funs) = GL (emp:empss) (funs + empFun emp)
