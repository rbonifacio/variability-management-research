
module ProductLineModel where

import UseCaseModel
import FeatureModel

data SPL = SPL {
	splFeatureModel :: FeatureModel,
	splUseCaseModel :: UseCaseModel
} deriving (Show)

data ProductInstance = ProductInstance {
	instanceConfiguration :: FeatureConfiguration,
	instanceUseCaseModel :: UseCaseModel
} deriving (Show)

emptyInstance :: SPL -> FeatureConfiguration -> ProductInstance
emptyInstance spl fc = 
 let 
 	ucm = splUseCaseModel spl
	name = ucmName ucm
 in ProductInstance {
 		instanceConfiguration = fc,
 		instanceUseCaseModel = UCM { ucmName = name, useCases = [], aspects = [] }
 	}
 	

type Model2Model = SPL -> ProductInstance -> ProductInstance