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
import System

parseArgs :: [String] -> IO ()
parseArgs args = 
 case head args of 
  "--fmIsSatisfiable" -> print "isSatisfiable"
  otherwhise -> print "error"
 
main :: IO ()
main = do
 args <- getArgs
 case args of 
  [] -> print "expecting one of the options: --fmIsSatifiable"
  (x:xs) -> parseArgs args
\end{code}

%include Types.lhs

%include FMTypeChecker.lhs

%include FCTypeChecker.lhs

\end{document}
 
