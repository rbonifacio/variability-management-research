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

replaceString :: String -> String -> String -> String
replaceString str1 str2 str3 = 
 unwords [if x == str2 then str3 ++ " " else x ++ " " | x <- words str1] 

firstOccurence :: Eq a => [a] -> a -> Int
firstOccurence [] c = 0
firstOccurence (x:xs) c =   
 if x == c 
  then 0
  else 1 + firstOccurence xs c  
  
findDelimitedString :: String -> Char -> Char -> [String] 
findDelimitedString str c1 c2 = 
 let pc1 = firstOccurence str c1 
     pc2 = firstOccurence str c2
     in 
      if (pc1 < (length str)) && (pc2 < (length str)) && (pc1 < pc2)
      	 then ([take (pc2-pc1-1) (drop (pc1+1) str)] ++ (findDelimitedString (drop (pc2+1) str) c1 c2))
      	 else []    

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