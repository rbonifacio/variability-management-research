\begin{code}

module ConfigurationKnowledge.Interpreter
where

import UseCaseModel.Types

import ConfigurationKnowledge.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, eval)

build :: FeatureModel -> FeatureConfiguration -> ConfigurationKnowledge -> UseCaseModel -> InstanceModel
build fm fc ck ucmodel = stepRefinement ts splmodel empty
 where 
  splmodel = (fm, ucmodel)
  ts = concat [transformations c| c <- ck, eval fc (expression c)]
  empty = (fc, ucmodel { useCases = [] , aspects = [] })	

stepRefinement :: [(SPLModel -> InstanceModel -> InstanceModel)] -> SPLModel -> InstanceModel -> InstanceModel
stepRefinement [] splmodel product = product
stepRefinement (x:xs) splmodel product = stepRefinement xs splmodel (x splmodel product)
 
\end{code}
    

 