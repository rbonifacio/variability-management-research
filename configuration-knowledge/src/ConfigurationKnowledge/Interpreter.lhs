\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  ConfigurationKnowledge.Interpreter
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Configuration Knowledge interpreter in Haskell.
--
-----------------------------------------------------------------------------
module ConfigurationKnowledge.Interpreter (build)
where

import UseCaseModel.Types
import RequirementModel.Types
import ConfigurationKnowledge.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, eval)

-- | Instantiates a product from the input models. 
--   In more details, it calls each transformation ('tasks', obtained 
--   from the configuration knowledge 'ck') that should be applied to 
--   the given feature configuration ('fc').

build :: FeatureModel                 -- ^ SPL feature model
      -> FeatureConfiguration         -- ^ selection of features, which characterizes the product
      -> ConfigurationKnowledge       -- ^ relationships between features and transformations
      -> SPLModel                     -- ^ SPL assets
      -> InstanceModel                -- ^ resulting instance of the build process
build fm fc ck spl = stepRefinement tasks spl emptyInstance
 where 
  tasks         = concat [transformations c| c <- ck, eval fc (expression c)]
  ucmodel       = splUCM spl
  emptyUCM      = ucmodel { useCases = [] , aspects = [] }
  emptyReq      = RM { reqs = [] }
  emptyInstance = InstanceModel fc emptyReq emptyUCM []
 	

stepRefinement :: [(SPLModel -> InstanceModel -> InstanceModel)] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splModel instanceModel = instanceModel
stepRefinement (x:xs) splModel instanceModel = stepRefinement xs splModel (x splModel instanceModel)
 
\end{code}
    

 