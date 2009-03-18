\begin{code}

module ConfigurationKnowledge.Interpreter
where

import ConfigurationKnowledge.Types
import FeatureModel.Types (FeatureConfiguration, eval)

build :: FeatureConfiguration -> ConfigurationKnowledge m -> m -> m
build fc ck model = stepRefinement [(x fc) | x <- t] model
 where t = concat [transformations c| c <- ck, eval fc (expression c)]	

stepRefinement :: [(m -> m)] -> m -> m
stepRefinement [] model = model
stepRefinement (x:xs) model = stepRefinement xs (x model)
 
\end{code}
    

 