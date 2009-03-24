\begin{code}

module ConfigurationKnowledge.Interpreter
where

import UseCaseModel.Types

import ConfigurationKnowledge.Types
import FeatureModel.Types (FeatureConfiguration, eval)

build :: FeatureConfiguration -> (ConfigurationKnowledge) -> UseCaseModel -> UseCaseModel
build fc ck splmodel = stepRefinement ts splmodel empty
 where 
  ts = concat [transformations c| c <- ck, eval fc (expression c)]
  empty = splmodel { useCases = [] , aspects = []}	

stepRefinement :: [(UseCaseModel -> UseCaseModel -> UseCaseModel)] -> UseCaseModel -> UseCaseModel -> UseCaseModel
stepRefinement [] splmodel product = product
stepRefinement (x:xs) splmodel product = stepRefinement xs splmodel (x splmodel product)
 
\end{code}
    

 