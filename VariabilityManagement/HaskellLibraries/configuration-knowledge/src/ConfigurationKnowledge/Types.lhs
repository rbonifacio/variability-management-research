\begin{code}

module ConfigurationKnowledge.Types
where

import UseCaseModel.Types

import FeatureModel.Types (FeatureConfiguration, FeatureExpression)

type ConfigurationKnowledge = [Configuration]
 

type Transformation = UseCaseModel -> UseCaseModel -> UseCaseModel
 
data Configuration = Configuration {
 expression :: FeatureExpression,
 transformations :: [Transformation] 	 
} 

\end{code}
    

 