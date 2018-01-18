{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lib where

import Employee
import Data.Tree
import Data.List

-- Problem 5 : make IO() part
someFunc :: IO ()
someFunc = readFile "company.txt" >>= (putStrLn.format.maxFun.read)

format :: GuestList -> String
format (GL emp fun) = m ++ (unlines n) 
  where
    m = "Total fun: " ++ show fun ++"\n"
    -- n = map (\x -> empName x) (sortBy compare emp)
    n = sortBy compare (map (\x -> empName x) emp)
    

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

-- Problem 4 : I need to find two best guest list: One with the boss and another without him
maxFun :: Tree Employee -> GuestList
maxFun tree = (\(x,y)->max x y) $ treeFold nextLevel tree
-- maxFun  (Node a xs) =  
--   where
--     m = nextLevel a (createTwo xs)
    
-- createTwo (Node m []) = [glCons m mempty,gl m mempty]
-- createTwo (Node m xs) = map (\y -> (m,glCons m y)) xs
