module Weaver where

import BasicTypes
import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment


ucmWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> UseCaseModel
ucmWeaver fm fc ck ucm = 
 let composedScenarios = (configure fc ck) 
 	 in UCM (ucmName ucm)
 	 	[uc | uc <- useCases ucm, length (ucScenarios (composedUc uc composedScenarios)) > 0]

scenarioWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> [StepList]
scenarioWeaver fm fc ck ucm = 
 if (length (validInstance fm fc)) > 0 
  then error "error..." 
  else allPathsFromScenarioList ucm (configure fc ck)
  
traceModelWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> Environment Feature -> [[String]]
traceModelWeaver fm fc ck ucm env = 
 computeAllTracesFromCompletePaths ucm env (scenarioWeaver fm fc ck ucm)
  
composedUc :: UseCase -> ScenarioList -> UseCase
composedUc uc composedScenarios = 
 UseCase (ucId uc) 
 		 (ucName uc)
 		 (ucDescription uc)
 		 ([s | s <- (ucScenarios uc), exists s composedScenarios]) 
  
-- traceModel env1 (steps scBuyProductBasic) 

-- length (compose fm fc01 configuration)  