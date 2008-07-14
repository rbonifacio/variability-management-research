module BasicTypes where 

import List

type Id = String
type Name = String
type Description = String
type Position = (Int, Int)

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

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
    if take (length find) s == find
        then repl ++ (replace (drop (length find) s) find repl)
        else [head s] ++ (replace (tail s) find repl)
        
replaceElement :: Eq a => a -> a -> [a] -> [a]
replaceElement x y [] = []
replaceElement x y (h:t) = 
  (if (x == h) then y else h) : (replaceElement x y t)
  
composeBefore :: Eq a => a -> [a] -> [a] -> [a]
composeBefore k [] _ = []
composeBefore k (h:t) l = 
 if (k == h) then l ++ (h:t)
 else h : composeBefore k t l     
 
composeAfter :: Eq a => a -> [a] -> [a] -> [a]
composeAfter k [] _ = []
composeAfter k (h:t) l = 
 if (k == h) then h: (l ++ t)
 else h : composeAfter k t l    
 
composeAround :: Eq a => a -> [a] -> [a] -> [a]
composeAround k [] _ = []
composeAround k (h:t) l = 
 if (k == h) then l ++ (t)
 else h : composeAround k t l  

firstElement :: Eq a => [a] -> a -> Maybe a
firstElement [] y = Nothing
firstElement (h:t) y = if (h == y) then Just h else firstElement t y

disjointConcatenation :: Eq a => [a] -> [a] -> [a]
disjointConcatenation l1 l2 = 
 l1 ++ [e | e <-l2 , (exists e l1) == False ]

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

concatValueList :: [String] -> String
concatValueList [] = ""
concatValueList (x:xs) = 
 if (length xs > 0)
  then x ++ " or " ++ concatValueList xs 
  else x  