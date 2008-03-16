module Weaver where

import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment



scenarioWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> [StepList]
scenarioWeaver fm fc ck ucm = 
 if (length (validInstance fm fc)) > 0 
  then error "error..." 
  else allPathsFromScenarioList ucm (configure fc ck)
  
traceModelWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> Environment Feature -> [[String]]
traceModelWeaver fm fc ck ucm env = 
 computeAllTracesFromCompletePaths ucm env (scenarioWeaver fm fc ck ucm)
  
-- traceModel env1 (steps scBuyProductBasic) 

-- length (compose fm fc01 configuration)  