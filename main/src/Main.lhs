\begin{code}

module Main 
where

import System

main :: IO ()
main = do
 args <- getArgs
 if (length args == 1) 
  then print "Sintax ok."
  else print "Command line error. Expecting build <path-to-build-file>"
\end{code}