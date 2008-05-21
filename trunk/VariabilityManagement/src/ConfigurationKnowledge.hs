{-
  
  ConfigurationKnowledge.hs
  
  This module describes a CK representation and 
  implements all functions related with it.
  
-}

module ConfigurationKnowledge where

import FeatureModel
import UseCaseModel

-- TODO: In the future, a configuration must be composed 
-- by a feature expression and a list of artifacts. 
type Configuration = (FeatureExpression, ScenarioList)

-- A configuration knowledge is a list of configurations.
-- Each configuration is composed by a feature expression and a 
-- list of artifacts (nowadays, a list of scenarios)
data ConfigurationKnowledge = CK [Configuration]

expression :: Configuration -> FeatureExpression
expression (e, sl) = e

scenarioList :: Configuration -> ScenarioList
scenarioList (e, sl) = sl

-- 
-- This function selects all artifacts whose configurations are valid 
-- for the feature configuration.
-- Usage: 
-- a) [scenario x | x<- (configure fc configuration)]
-- b) computeAllTracesFromScenarioList env1 (configure fc configuration) 
selectScenarios :: FeatureConfiguration -> ConfigurationKnowledge -> ScenarioList
selectScenarios fc (CK []) = [idle]
selectScenarios fc (CK (x:xs)) = 
 if (eval fc (expression x)) 
  then (scenarioList x) ++ (selectScenarios fc (CK xs))
  else selectScenarios fc (CK xs) 
  
  