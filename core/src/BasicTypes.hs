module BasicTypes
where 

import qualified System.FilePath.Posix as P
import qualified System.FilePath.Windows as W
 
import Control.Monad (MonadPlus(..), liftM)

import List

type Id = String
type Name = String


-- | A generic data type for dealing with parsers.
--   Actually, this is an idiom, being present in several Haskell parsers.
--   Since parsers usually have to perform some kind of IO, the ParserResult 
--   data type shuld be an instance of Monad.
-- 
--   As a final remark, the result of a parser might be either a Sucess or a Fail.
    
data ParserResult a = Success a | Fail String
 deriving (Read, Show, Eq, Ord)

instance Monad ParserResult where 
 return = Success
 fail = Fail
 Success a >>= f = f a
 Fail s    >>= f = Fail s

instance Functor ParserResult where 
 fmap = liftM

instance MonadPlus ParserResult where
  mzero = Fail "Err.mzero"
  mplus (Fail _) y = y
  mplus x _ = x

isSuccess :: ParserResult a -> Bool
isSuccess (Success _) = True
isSuccess (Fail _) = False

-- 

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


createURI :: String -> String
createURI f@(x:':':ys) = createFromWindowsPath f
createURI f = createFromPosixPath f 
  
createFromWindowsPath f = 
 let 
  driver = W.takeDrive f
  directories = replaceString " " "/" (unwords (tail (W.splitDirectories f)))
 in  
  "file:///" ++ ( (head driver) : ":/" ) ++ directories 

createFromPosixPath f = "file://" ++ f  

getFileExtension :: String -> String 
getFileExtension = P.takeExtension  