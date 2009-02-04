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

\author{Rodrigo Bonifacio \\ rbonifacio@@computer.org}

\begin{document}

\maketitle

This document describes the Haskell library for feature modeling. 

%include FeatureModel.lhs

\begin{code}
module Main where
import FeatureModel.FeatureModel
\end{code} 

 
