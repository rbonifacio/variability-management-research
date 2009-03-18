\begin{code}

module ConfigurationKnowledge.Types
where

import FeatureModel.Types (FeatureConfiguration, FeatureExpression)

type ConfigurationKnowledge m = [Configuration m]

type Transformation m = FeatureConfiguration -> m -> m
 
data Configuration m = Configuration {
 expression :: FeatureExpression,
 transformations :: [Transformation m] 	 
}

\end{code}
    

 