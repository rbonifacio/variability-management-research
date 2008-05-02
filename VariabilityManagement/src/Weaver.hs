module Weaver where

import BasicTypes
import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment
import List


ucmWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> UseCaseModel
ucmWeaver fm fc ck ucm = 
 let composedScenarios = (configure fc ck) 
 	 in UCM (ucmName ucm)
 	 	([(composedUc uc composedScenarios)| uc <- useCases ucm, length (ucScenarios (composedUc uc composedScenarios)) > 0] ++ [ucIddle])

scenarioWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> [StepList]
scenarioWeaver fm fc ck ucm = 
 let composedScenarios = (configure fc ck) 
 	in if (length (validInstance fm fc)) > 0 
  		then error "error..." 
  		else nub (allPathsFromScenarioList (ucmWeaver fm fc ck ucm) composedScenarios)
  
traceModelWeaver :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> Environment Feature -> [[String]]
traceModelWeaver fm fc ck ucm env = 
 let composedScenarios = (configure fc ck) 
 in computeAllTracesFromCompletePaths (ucmWeaver fm fc ck ucm) env (scenarioWeaver fm fc ck ucm)
  
composedUc :: UseCase -> ScenarioList -> UseCase
composedUc uc composedScenarios = 
 UseCase (ucId uc) 
 		 (ucName uc)
 		 (ucDescription uc)
 		 ([s | s <- (ucScenarios uc), exists s composedScenarios]) 
  
-- traceModel env1 (steps scBuyProductBasic) 

-- length (compose fm fc01 configuration)

--let ucm02 = ucmWeaver fm01 fc01 ck01 ucm01 
-- in allPathsFromScenarioList ucm02 (concatMap (ucScenarios) [x | x <- useCases ucm02])
-- []
  