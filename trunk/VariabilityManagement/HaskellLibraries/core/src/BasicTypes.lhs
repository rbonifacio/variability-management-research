\begin{code}
module BasicTypes
where 

import List

type Id = String
type Name = String

split :: Char -> String -> [String]
split c "" = [""] 
split c s = 
 if any (== c) s then 
  (delete c (takeWhile (/= c) s)) : split c (tail (dropWhile (/= c) s))
 else [s]
 
splitAndRemoveBlanks :: Char -> String -> [String] 
splitAndRemoveBlanks c s = [filter (/= ' ') x | x <- (split c s) ] 

existsWord:: String -> String -> Bool 
existsWord w s  = 
 let ws = parseString s
 in  w `elem` ws

replaceString :: String -> String -> String -> String
replaceString old new str = 
 let ws = parseString str
 in unparseString (replaceString' old new ws)
 where 
  replaceString' o n [] = []
  replaceString' o n (w:ws) = (if (w == o) then n else w) : replaceString' o n ws


concatBefore :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
concatBefore _ _ [] = []
concatBefore f ls (x:xs) = 
 if f x 
  then ls ++ (x:xs) 
  else x : (concatBefore f ls xs)

concatAfter :: Eq a => (a -> Bool) -> [a] -> [a] -> [a]
concatAfter _ _ [] = []
concatAfter f ls (x:xs) = 
 if f x
  then x : (ls ++ xs)
  else x : (concatAfter f ls xs)

parseString :: String -> [String]
parseString s  = parseString' (s, "", [])

parseString' :: (String, String, [String]) -> [String]
parseString' ("", s2, ss) = (ss ++ [s2])
parseString' ((x:xs), s2, ss) = 
   if x `elem` [',', '.', ' ', '-'] 
    then parseString' (xs, "", (ss ++ [s2]) ++ [(x:"")])
    else parseString' (xs, s2 ++ (x:""), ss)

unparseString :: [String] -> String 
unparseString xs = concat xs
  
\end{code}