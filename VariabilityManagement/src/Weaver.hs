module Weaver where

import BasicTypes
import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment
import List


-- 
-- Given a product instance (a feature configuration),
-- a configuration knowledge and a SPL use case model, 
-- this function returns a product specific use case model. 
-- 
-- Only use cases that have at least one scenario selected 
-- will be present in the resulting use case model. 
-- 
selectUseCases :: FeatureModel -> FeatureConfiguration -> 
				  ConfigurationKnowledge -> UseCaseModel -> 
				  UseCaseModel
selectUseCases fm fc ck ucm = 
 let selectedScenarios = (selectScenarios fc ck) 
  	in UCM (ucmName ucm) (selectUseCasesFromUCM ucm selectedScenarios)

-- 
-- Given a product instance (a feature configuration), 
-- a configuration knowledge and a SPL use case model, 
-- this function returns lists of composed scenarios.
-- 
-- TODO: all type checking should be performed before 
-- composing scenarios. 
-- 
scenarioComposition :: FeatureModel -> FeatureConfiguration -> 
					   ConfigurationKnowledge -> UseCaseModel -> 
					   [StepList]
scenarioComposition fm fc ck ucm = 
 let selectedScenarios = (selectScenarios fc ck) 
 	in if (length (validInstance fm fc)) > 0 
  	 then error "error..." 
  	 else nub (allPathsFromScenarioList (selectUseCases fm fc ck ucm) selectedScenarios)
  	 
-- 
-- This function is responsible for binding 
-- the parameters of scenario list. Such a scenario 
-- list is represented, in this version, just as a list 
-- of step sequences. 
--
bindAllParameters :: FeatureConfiguration -> [StepList] -> 
					 (Environment Feature) ->  
					 [StepList]
bindAllParameters fc stepSequences env = 
  [bindParameterFromSequence fc x env | x <- stepSequences]


-- 
-- This auxiliarly function returns all use cases, from a 
-- use case model, which have scenarios present in the list 
-- of scenarios passed as the second argument.
-- 
selectUseCasesFromUCM :: UseCaseModel -> ScenarioList -> [UseCase]
selectUseCasesFromUCM ucm selectedScenarios = 
 [selectedUseCase uc selectedScenarios | uc <-useCases ucm, shouldSelectUC uc selectedScenarios] 
 	 
--
-- This auxiliarly function returns true if 
-- a use case has at least one scenario present 
-- in the second parameter. The list of selected 
-- scenarios for a specific product may be passed 
-- as second argument.
-- 
shouldSelectUC :: UseCase -> ScenarioList -> Bool
shouldSelectUC uc selectedScenarios = 
 let scenarios = [s | s <- (ucScenarios uc), exists s selectedScenarios] 
  in (length scenarios > 0)
 
-- 
-- This auxiliarly function returns a use case 
-- composed by scenarios that are present in the 
-- second parameter.
-- 
selectedUseCase :: UseCase -> ScenarioList -> UseCase
selectedUseCase uc selectedScenarios = 
 let scenarios = [s | s <- (ucScenarios uc), exists s selectedScenarios] 
  in UseCase (ucId uc) (ucName uc) (ucDescription uc) scenarios
 
bindParameterFromSequence :: FeatureConfiguration -> StepList -> (Environment Feature) -> StepList
bindParameterFromSequence fc [] env = []
bindParameterFromSequence fc (x:xs) env = 
 if (hasParameters x) 
  then
   x : (bindParameterFromSequence fc xs env)
  else  
   (extractParameterValuesFromStep x env) : (bindParameterFromSequence fc xs env)
 
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
-- This auxiliarly function returns all name of parameters from 
-- a given step. If the step has no parameters, the returning 
-- list is empty.
--  
extractParametersFromStep :: Step -> [String]
extractParametersFromStep step = 
 let str = unwords [(action step), (state step), (response step)] 
 	in findDelimitedString str '<' '>'  

-- 
-- This auxiliarly function returns returns a step with 
-- all of its parameters resolved.
-- 
extractParameterValuesFromStep :: Step -> Environment Feature -> Step
extractParameterValuesFromStep step env = 
 let parameters = extractParametersFromStep step
 	in Step (stepId step) 
  		    (owner step) 
  		    (replaceParameters env (action step) parameters) 
  		    (replaceParameters env (state step) parameters)
  		    (replaceParameters env (response step) parameters)
  		    (annotations step)

replaceParameters :: Environment Feature -> String -> [String] -> String
replaceParameters env s [] = s 
replaceParameters env s (x:xs) =
 let values y = concatValueList (optionValues  (featureOptions (hash env y)))
  in replaceParameters env (replace s x (values x)) xs  	    

-- 
-- This auxiliarly function is reponsible for returning
-- an appropriate representation for the values of a parameter.
-- 
-- Example: [p1, p2] => (p1 or p2) 
concatValueList :: [String] -> String
concatValueList values = "(" ++ (replace (unwords values) " " " or ") ++ ")"


-- 
-- This version of extractParameterValues 
-- returns a list of steps with all valid parameters' 
-- configurations
-- 
-- extractParameterValuesFromStep :: Step -> Environment Feature -> [Step]
-- extractParameterValuesFromStep step env = 
-- let parameters = extractParametersFromStep step 
-- 	in [bindParametersFromStep step env parameter | parameter <- parameters]

-- 
-- This version of bindParametersFromStep
-- returns one step for each parameter value.
-- A useful version when mapping for test cases.
-- 

--bindParametersFromStep :: Step -> Environment Feature -> String -> [Step]
--bindParametersFromStep step env parameter =  
-- let values = optionValues (featureOptions (hash env parameter))
--     p = "<" ++ parameter ++ ">" 
--     a = action step
--     s = state step
--     r = response step
--  in [Step (stepId step) 
--  		   (owner step) 
--  		   (replaceString a p value) 
--  		   (replaceString s p value)
--  		   (replaceString r p value)
--  		   (annotations step)
--  	  | value <- values]  
     
