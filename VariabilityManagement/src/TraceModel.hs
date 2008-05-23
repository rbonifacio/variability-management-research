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
import BasicTypes

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

type Trace = [String]
type TraceModel = [Trace]

computeTraceModel :: [StepList] -> TraceModel
computeTraceModel [] = [] 
computeTraceModel (x:xs) = (computeTrace x) ++ (computeTraceModel xs)   

computeTrace :: StepList -> TraceModel
computeTrace [] = [[]]
computeTrace (x:xs) = [] : (stepId x) ^ (computeTrace (xs))

traceRefinement :: TraceModel -> TraceModel -> Bool
traceRefinement t [] = False
traceRefinement [] referenceModel = True
traceRefinement (x:xs) referenceModel = 
 if (exists x referenceModel) 
  then traceRefinement xs referenceModel
  else False




