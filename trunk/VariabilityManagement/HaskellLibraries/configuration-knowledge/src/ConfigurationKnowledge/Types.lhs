\begin{code}

module ConfigurationKnowledge.Types
where

import UseCaseModel.Types

import FeatureModel.Types (FeatureModel, FeatureConfiguration, FeatureExpression)

type ConfigurationKnowledge = [Configuration]

type SPLModel = (FeatureModel, UseCaseModel)
type InstanceModel = (FeatureConfiguration, UseCaseModel)

type Transformation = SPLModel -> InstanceModel -> InstanceModel
 
data Configuration = Configuration {
 expression :: FeatureExpression,
 transformations :: [Transformation] 	 
} 

\end{code}
    

 