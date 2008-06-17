{-
  
  ConfigurationKnowledge.hs
  
  This module describes a CK representation and 
  implements all functions related with it.
  
-}

module ConfigurationKnowledge 
 (ConfigurationKnowledge, Configuration, Model2Model, buildConfiguration) 
where

import AbstractModel
import FeatureModel

type ConfigurationKnowledge model = [Configuration model]

data Configuration model = Configuration {
 expression :: FeatureExpression,
 transformations :: [Model2Model model] 	 
}

buildConfiguration :: (Model m) => m -> FeatureConfiguration -> ConfigurationKnowledge m -> m
buildConfiguration im fc ck = applyAllTransformations im fc ck (emptyModel im) 
    
applyAllTransformations im fc [] om = om
applyAllTransformations im fc (x:xs) om = 
 let t = (transformations x) 
  in if (eval fc (expression x)) 
      then applyAllTransformations im fc xs (applyTransformations im t om) 
      else applyAllTransformations im fc xs om
 
applyTransformations :: (Model m) => m -> [Model2Model m] -> m -> m
applyTransformations im [] om = om
applyTransformations im (x:xs) om = applyTransformations im xs (x im om) 





  
  