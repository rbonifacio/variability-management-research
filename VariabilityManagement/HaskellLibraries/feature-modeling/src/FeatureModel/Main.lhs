%
% This is the Literate Haskell that defines the main module of the 
% FeatureModeling library.
% 
% author: Rodrigo Bonifacio - rbonifacio@computer.org
%
% 2008/2009
%


\documentclass[12pt]{article}
\usepackage{a4wide}

%include polycode.fmt
%include lhs2tex.sty

%format |=> = "\Rightarrow"


\title{A Haskell Library for Feature Modeling}

\author{Rodrigo Bonifacio and Paulo Borba \\ {rba2,phmb}@@cin.ufpe.br}

\begin{document}

\maketitle

\begin{abstract}
We present a Haskell library for feature modeling. Among other capabilities, 
using this library, we are able to: (a) import a feature model generated using the 
Feature Modeling plugin, (b) check if a feature model is satisfiable, and (c) 
check if a product configuration is a valid instance of a feature model. Additionally, we 
argue that extending this library to support some types of \emph{reasoning about feature models}  
would be straightforward. We assume that a reader of this report has a basic knowledge on 
feature models.   
\end{abstract}

\section{Introduction}
In order to test this library, we provide a command line application. The following options 
might be used for running it:

\begin{description}
\item[Feature Model test of satisfiability:] enter --fmIsSatisfiable <path-to-feature-model>
\item[Feature Model type checker:] enter --fmTypeChecker <path-to-feature-model>
\item[Feature Configuration type checker:] enter --fcTypeChecker <path-to-feature-model> <path-to-feature-configuration>
\end{description}

\begin{code}
module Main where

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

import Text.XML.HXT.Arrow

import Maybe

import System
import System.Console.GetOpt

-- execIsSatisfiable :: [String] -> IO ()
-- execIsSatisfiable []  = print "expecting a file name. try --help"
-- execIsSatisfiable [s] = 
--  do 
--      [x] <- runX ( xunpickleDocument xpFeature [ (a_validate,v_0)
--                                     , (a_trace, v_1)
--                                     , (a_remove_whitespace,v_1)
--                                     , (a_preserve_comment, v_0)
--                                      ] s);
--      print s
 
-- execIsSatisfiable otherwhise = print "command line error. try --help"

execSummary :: FeatureModel -> IO ()
execSummary fmodel = do print $ summary fmodel


execCheck1 :: FeatureModel -> IO () 
execCheck1 fmodel = do print $ fmTypeChecker fmodel

main :: IO ()
main = do
 args <- getArgs
 case getOpt Permute options args of
  ( [], [], [] ) -> error $ usageInfo header options
  ( flags, [] , [] ) -> processFlags flags
  ( _, nonOpts, [] ) -> error $ concat ["\nunrecognized options ", unwords nonOpts, (usageInfo header options)]
  ( _, _, msgs) -> error $ concat msgs ++ usageInfo header options

processFlags :: [Flag] -> IO ()
processFlags flags = do
 let args = getOptions flags defaultOptions
 fmodel <- parseFeatureModel args
 case (cmd args) of
  "summary" -> execSummary (fmodel)
  "check1"  -> execCheck1  (fmodel)
  "check2"  -> execCheck1  (fmodel)
  otherwise -> error $ concat ["\nunrecognized command ", (cmd args), (usageInfo header options)]


parseFeatureModel args = do
 x <- readFile (fm args) 
 let fm = case (fmt args) of 
           "fmp"   -> translateToFm (pGrammar (myLexer x)) 
           "fmide" -> translateToFm (pGrammar (myLexer x))
           otherwise -> error $ concat ["\nunrecognized format ", (fmt args), (usageInfo header options)]
 return fm

translateToFm (Ok g)  = grammarToFeatureModel g
translateToFm (Bad s) = error s

data Flag = Format String 
          | Command String 
          | FMFile String 
          | FCFile String
 deriving (Show)

options :: [OptDescr Flag]
options = [ 
  Option ['f'] ["format"] 
         (OptArg optformat "FMT") 
         ("Set the format option. Use (fmide) for FMIde models" ++
          "\nor (fmp) for FMPlugin models. The defaul option" ++
         "\nis fmp.\n\n") , 
  Option ['c'] ["command"] 
         (ReqArg Command "CMD")
         ("Identify the command to be performed. Use \"summary\" " ++
          "\nfor presenting a summary of the specified feature model" ++
          "\nUse \"check1\" for checking the specified feature model.  " ++
          "\nFinally, use \"check2\" for checking the specified " ++  
          "\nfeature model and feature configuration.\n\n"),
  Option [] ["file1"] 
         (ReqArg FMFile "FILE")
         "Identify the feature model file.\n\n",
  Option [] ["file2"] 
         (ReqArg FCFile "FILE")
         "Identify the feature configuration file.\n\n"
 ]

optformat :: Maybe String -> Flag
optformat = Format . fromMaybe "fmp" 

data Options = Options {
  fmt :: String,
  cmd :: String,
  fm  :: String,
  fc  :: String
} deriving (Show)

defaultOptions = Options {
  fmt = "fmp",
  cmd = "",
  fm  = "",
  fc = ""
}

getOptions [] options = options
getOptions (x:xs) options = 
 case x of  
  (Format s)  -> getOptions xs (options { fmt = s })
  (Command s) -> getOptions xs (options { cmd = s })
  (FMFile s)  -> getOptions xs (options { fm  = s })
  (FCFile s)  -> getOptions xs (options { fc  = s })

header = "\nUsage: hfm [OPTION...]\n"
\end{code}

%include Types.lhs

%include FMTypeChecker.lhs

%include FCTypeChecker.lhs

\end{document}
 
