\begin{code}

module ConfigurationKnowledge.Interpreter (build)
where

import UseCaseModel.Types

import ConfigurationKnowledge.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, eval)

-- | Instantiates a product from the input models. 
--   In more details, it calls each transformation ('ts', obtained 
--   from the configuration knowledge 'ck') that should be applied to 
--   the given feature configuration ('fc').

build :: FeatureModel                 -- ^ SPL feature model
      -> FeatureConfiguration         -- ^ selection of features, which characterizes the product
      -> ConfigurationKnowledge       -- ^ relationships between features and transformations
      -> UseCaseModel                 -- ^ SPL use case model
      -> InstanceModel                -- ^ resulting instance of the build process
build fm fc ck ucmodel = stepRefinement ts splmodel empty
 where 
  splmodel = (fm, ucmodel)
  empty    = (fc, ucmodel { useCases = [] , aspects = [] })
  ts       = concat [transformations c| c <- ck, eval fc (expression c)]	

stepRefinement :: [(SPLModel -> InstanceModel -> InstanceModel)] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splmodel product = product
stepRefinement (x:xs) splmodel product = stepRefinement xs splmodel (x splmodel product)
 
\end{code}
    

 