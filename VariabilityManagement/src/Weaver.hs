module Weaver where

import BasicTypes
import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment
import List


-- ***********************************************************
-- Given a product instance (a feature configuration),
-- a configuration knowledge and a SPL use case model, 
-- this function returns a product specific use case model. 
-- 
-- Only use cases that have at least one scenario selected 
-- are present in the resulting use case model. 
-- ***********************************************************
selectUseCases :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> UseCaseModel 
selectUseCases fm fc ck ucm = 
 let selectedScenarios = (selectScenarios fc ck) 
  	in UCM (ucmName ucm) (selectUseCasesFromUCM ucm selectedScenarios)

-- **********************************************************
-- Given a product instance (a feature configuration), 
-- a configuration knowledge and a SPL use case model, 
-- this function returns lists of woven scenarios. 
-- **********************************************************
scenarioComposition :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> [StepList]
scenarioComposition fm fc ck ucm = 
 let selectedScenarios = (selectScenarios fc ck) 
 	in if (length (validInstance fm fc)) > 0 
  	 then error "error..." 
  	 else nub (allPathsFromScenarioList (selectUseCases fm fc ck ucm) selectedScenarios)
  	 

selectUseCasesFromUCM :: UseCaseModel -> ScenarioList -> [UseCase]
selectUseCasesFromUCM ucm selectedScenarios = 
 [selectedUseCase uc selectedScenarios | uc <-useCases ucm, shouldSelectUC uc selectedScenarios] 
 	 

selectedUseCase :: UseCase -> ScenarioList -> UseCase
selectedUseCase uc selectedScenarios = 
 let scenarios = [s | s <- (ucScenarios uc), exists s selectedScenarios] 
 in UseCase (ucId uc) (ucName uc) (ucDescription uc) scenarios


shouldSelectUC :: UseCase -> ScenarioList -> Bool
shouldSelectUC uc selectedScenarios = 
 let scenarios = [s | s <- (ucScenarios uc), exists s selectedScenarios] 
 in (length scenarios > 0)
 		

--
-- Given a feature configuration, a list of scenarios 
-- and a mapping that relates parameters to features, 
-- this function resolves all parameters declared in the 
-- scenarios.   		
--   

bindAllParameters :: FeatureConfiguration -> [StepList] -> (Environment Feature) ->  [StepList]
bindAllParameters fc stepSequences env = 
 plainList [bindParameterFromSequence fc x env | x <- stepSequences]
 
bindParameterFromSequence :: FeatureConfiguration -> StepList -> (Environment Feature) -> [StepList]
bindParameterFromSequence fc [] env = []
bindParameterFromSequence fc (x:xs) env = 
 if (hasParameters x) 
  then
   [[x]] +++ (bindParameterFromSequence fc xs env)
  else  
   [[]] +++ (bindParameterFromSequence fc xs env)
 
 
  


traceModelWeaver :: 
 FeatureModel -> FeatureConfiguration -> 
 ConfigurationKnowledge -> UseCaseModel -> 
 Environment Feature -> 
 [[String]]

traceModelWeaver fm fc ck ucm env = 
 let selectedScenarios = (selectScenarios fc ck) 
 in computeAllTracesFromCompletePaths (selectUseCases fm fc ck ucm) env (scenarioComposition fm fc ck ucm)

-- 
-- This auxiliarly function returns true if a step has parameters. 
-- Otherwise, returns false.
--
hasParameters :: Step -> Bool
hasParameters step = 
 length (extractParametersFromStep (step)) > 0

-- 
-- This auxiliarly function return all name of parameters from 
-- a given step. If the step has no parameters, the returning 
-- list is empty.
--  
extractParametersFromStep :: Step -> [String]
extractParametersFromStep step = 
 let str = unwords [(action step), (state step), (response step)] 
 	in findDelimitedString str '<' '>'  


extractParameterValuesFromStep ::  Step -> Environment Feature -> [Step]
extractParameterValuesFromStep step env = 
 let parameters = extractParametersFromStep step 
 	in plainList [bindParametersFromStep step env parameter | parameter <- parameters]

bindParametersFromStep :: Step -> Environment Feature -> String -> [Step]
bindParametersFromStep step env parameter =  
 let values = optionValues (featureOptions (hash env parameter))
     p = "<" ++ parameter ++ ">" 
     a = action step
     s = state step
     r = response step
  in [Step (stepId step) 
  		   (owner step) 
  		   (replaceString a p value) 
  		   (replaceString s p value)
  		   (replaceString r p value)
  		   (annotations step)
  	  | value <- values]  
     
 