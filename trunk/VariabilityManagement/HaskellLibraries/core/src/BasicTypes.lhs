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
 let ws = words s
 in  w `elem` ws

replaceString :: String -> String -> String -> String
replaceString old new str = 
 let ws = words str
 in unwords (replaceString' old new ws)
 where 
  replaceString' o n [] = []
  replaceString' o n (w:ws) = (if (w == o) then n else w) : replaceString' o n ws
 
\end{code}