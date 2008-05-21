module BasicTypes where 

import List

type Id = String
type Name = String
type Description = String

exists :: Eq a => a -> [a] -> Bool
exists e [] = False
exists e (x:xs) = 
 if e == x 
  then True 
  else exists e xs
  
existAll :: Eq a => [a] -> [a] -> Bool
existAll [] _ = True
existAll (x:xs) [] = False
existAll (x:xs) (y:ys) = (exists x (y:ys)) && (existAll xs (y:ys))  

-- *************************************************************
-- Operator that distribute the concat operator over lists.
-- The result is:
-- [[1M, 2M, 3M], [A11, A12]] +++ [[A21, A22, A23]] = 
-- [[1M, 2M, 3M, A21, A22, A23], [A11, A12, A21, A22, A23]] 
-- *************************************************************

(+++) :: [[a]] -> [[a]] -> [[a]]
steps1 +++ steps2 = [ x ++ y | x<-steps1 , y <- steps2]

plainList :: [[a]] -> [a]
plainList [] = [] 
plainList (x:xs) = x ++ plainList(xs)

split :: Char -> String -> [String]
split c "" = [""] 
split c s = 
 if any (== c) s then 
  (delete c (takeWhile (/= c) s)) : split c (tail (dropWhile (/= c) s))
 else [s]
 
splitAndRemoveBlanks :: Char -> String -> [String] 
splitAndRemoveBlanks c s = [filter (/= ' ') x | x <- (split c s) ] 