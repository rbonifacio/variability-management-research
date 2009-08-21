\begin{code}

import FeatureModel.Types
import FeatureModel.FMTypeChecker
import FeatureModel.FCTypeChecker

import FeatureModel.Parsers.FMPlugin.XmlFeatureParser 

import FeatureModel.Parsers.FMIde.FMIde2FeatureModel
import FeatureModel.Parsers.FMIde.AbsFMIde
import FeatureModel.Parsers.FMIde.SkelFMIde
import FeatureModel.Parsers.FMIde.ErrM
import FeatureModel.Parsers.FMIde.LexFMIde
import FeatureModel.Parsers.FMIde.ParFMIde

import System

import System.IO

import Test.BenchPress




main :: IO()
main = bench 10 $ do
  args <- getArgs 
  x    <- readFile (head args) 
  let y   = pGrammar (myLexer x)
  let fm  = translateToFm y
  let cnf = fmToCNFExpression fm
  print "done"

translateToFm (Ok g) = grammarToFeatureModel g

\end{code}