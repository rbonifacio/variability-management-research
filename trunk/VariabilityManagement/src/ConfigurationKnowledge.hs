{-
  
  ConfigurationKnowledge.hs
  
  This module describes a CK representation and 
  implements all functions related with it.
  
-}

module ConfigurationKnowledge 
 --(ConfigurationKnowledge, Configuration, Model2Model, buildConfiguration) 
where

import AbstractModel
import FeatureModel

type ConfigurationKnowledge model = [Configuration model]
 
data Configuration model = Configuration {
 expression :: FeatureExpression,
 transformations :: [Model2Model model] 	 
}

buildConfiguration :: (AbstractModel m) => m -> FeatureConfiguration -> ConfigurationKnowledge m -> m
buildConfiguration im fc ck = applyAllTransformations im fc ck (emptyModel im) 
    
applyAllTransformations im fc [] om = om
applyAllTransformations im fc (x:xs) om = 
 let t = (transformations x) 
  in if (eval fc (expression x)) 
      then applyAllTransformations im fc xs (applyTransformations im fc t om) 
      else applyAllTransformations im fc xs om
 
applyTransformations :: (AbstractModel m) => m -> FeatureConfiguration -> [Model2Model m] -> m -> m
applyTransformations im fc [] om = om
applyTransformations im fc (x:xs) om = 
 case x of 
  (ConsM2MType0 _) -> applyTransformations im fc xs (fnModel0 (x) om)
  (ConsM2MType1 _) -> applyTransformations im fc xs (fnModel1 (x) im om)
  (ConsM2MType2 _) -> applyTransformations im fc xs (fnModel2 (x) fc om)
  (ConsM2MType3 _) -> applyTransformations im fc xs (fnModel3 (x) fc im om) 





  
  