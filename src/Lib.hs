{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib where

import Employee
import Data.Tree

someFunc :: IO ()
someFunc = putStrLn "someFunc"

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list fun) = (GL (emp:list) ((empFun emp)+fun))

instance Monoid GuestList where
  mempty = (GL [] 0)
  mappend (GL list1 fun1) (GL list2 fun2) = GL (list1++list2) (fun1+fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2)
  | fun1>fun2 = gl1
  | otherwise = gl2

-- Problem 2
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go where
    go (Node x ts) = f x (map go ts)
-- treeFold f i tree = (foldl (f (rootLabel tree)) i (subForest tree))
--
--
--
-- Problem 3 : I need to find two best guest list: One with the boss and another without him
nextLevel :: Employee -> [(GuestList,GuestList)] -> (GuestList,GuestList)
nextLevel emp list = (nx,ny)
  where 
    (wIth,wOut) = unzip list
    m = (map (glCons emp) wIth)
    nx = foldl moreFun mempty m
    ny = foldl moreFun mempty wOut
