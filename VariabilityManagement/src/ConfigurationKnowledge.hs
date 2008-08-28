{-
  
  ConfigurationKnowledge.hs
  
  This module describes a CK representation and 
  implements all functions related with it.
  
-}

module ConfigurationKnowledge 
 --(ConfigurationKnowledge, Configuration, Model2Model, buildConfiguration) 
where

import BasicTypes
import ProductLineModel
import FeatureModel

type ConfigurationKnowledge = [Configuration]
 
data Configuration  = Configuration {
 expression :: FeatureExpression,
 transformations :: [Model2Model] 	 
}


build :: SPL -> FeatureConfiguration -> ConfigurationKnowledge -> ProductInstance
build spl fc ck = 
 let 
 	t = concat [transformations c| c <- ck, eval fc (expression c)]
 	i = (emptyInstance spl fc)   	
 in stepRefinement [(x spl) | x <- t] i
    

 