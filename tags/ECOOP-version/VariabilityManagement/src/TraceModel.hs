{-
   
   TraceModel.hs

   This module defines all functions related 
   to the implementation of a trace model 
   computed from  
   a scenario (or set of steps). 

   Author: Rodrigo Bonifacio 
	
-}

module TraceModel where -- (traceModel) where --, computeAllTraces) where

import Prelude hiding ( (^) )
import List 
import FeatureModel
import UseCaseModel 
import Environment

-- **********************************************************
-- Trace model function definition
-- 
-- Description:
--
-- Compute the trace model of a set of steps. The notion of 
-- CSP channels are considered here. A channel corresponds 
-- to a possible alternativeFeature or orFeature referenced 
-- by a parameter in the step specification.
--
-- Usage: [traceModel x | x <- (completePaths scenario1)]
-- **********************************************************

traceModel :: Environment Feature -> [Step] -> [[String]]
traceModel _ [] = [[]]
traceModel e (x:xs) = [] : (bind e x) ^ (traceModel e (xs))


bind :: Environment Feature -> Step -> String
bind e x =  
 if (length (extractParameters (details x)) == 0)
  then stepId x
  else stepId x ++ (concatStrList (extractParameterValues e x))

extractParameterValues :: Environment Feature -> Step -> [String]
extractParameterValues e s = 
 [optionValues (featureOptions y) | y <- [hash e x | x <- (nub (extractParameters (details s)))]]

concatStrList :: [String] -> String
concatStrList [] = []
concatStrList (x:xs) = "(" ++ x ++ ")" ++ (concatStrList xs) 

-- 
-- Extract parameters from a String
-- Something like: 
-- "Select a <MessageType> from the CreateMessage menu".
--
-- This will result in the [MessageType] list. An 
-- environment must be used to retrive the correctly 
-- feature.
--
extractParameters :: String -> [String]
extractParameters str = 
 [delete '<' (delete '>' x) | x <- (words str), head(x) == '<', last(x) == '>'] 


-- 
-- Compute all traces from a list of scenarios
--
-- Usage: computeAllTracesFromScenarioList ucm env1 [scenario1,scenario2,scenario3,scenario4]
--
computeAllTracesFromScenarioList :: UseCaseModel -> Environment Feature -> [Scenario] -> [[String]]
computeAllTracesFromScenarioList ucm env sl = nub (computeAllTracesFromCompletePaths ucm env (allPathsFromScenarioList ucm sl)) 

computeAllTracesFromCompletePaths :: UseCaseModel -> Environment Feature -> [StepList] -> [[String]]
computeAllTracesFromCompletePaths ucm env [] = []
computeAllTracesFromCompletePaths ucm env (x:xs) = (traceModel env x) ++ (computeAllTracesFromCompletePaths ucm env xs) 

allPathsFromScenarioList :: UseCaseModel -> [Scenario] -> [StepList] 
allPathsFromScenarioList ucm [] = []
allPathsFromScenarioList ucm (x:xs) = (completePaths ucm x) ++ (allPathsFromScenarioList ucm xs)

