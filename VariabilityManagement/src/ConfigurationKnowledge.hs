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
    


  
--
--buildConfiguration :: ProductLine -> FeatureConfiguration -> ConfigurationKnowledge -> ProductInstance
--buildConfiguration spl fc ck = applyAllTransformations spl ck (emptyInstance spl fc)
    
--applyAllTransformations spl [] productInstance = productInstance
--applyAllTransformations spl (x:xs) productInstance = 
-- let
-- 	f = (instanceConfiguration productInstance)
-- 	e = (expression x) 
-- 	t = (transformations x)
--   in if (eval f e) 
--      then applyAllTransformations spl xs (applyTransformations spl t productInstance) 
--      else applyAllTransformations spl xs productInstance
-- 
--applyTransformations :: ProductLine -> [Model2Model] -> ProductInstance -> ProductInstance
--applyTransformations spl [] productInstance = productInstance
--applyTransformations spl (x:xs) productInstance = applyTransformations spl xs (x spl productInstance)


  
  