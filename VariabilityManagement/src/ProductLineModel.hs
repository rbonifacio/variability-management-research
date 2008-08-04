
module ProductLineModel where

import UseCaseModel
import FeatureModel

data ProductLine = ProductLine {
	splFeatureModel :: FeatureModel,
	splUseCaseModel :: UseCaseModel
}

data ProductInstance = ProductInstance {
	instanceConfiguration :: FeatureConfiguration,
	instanceUseCaseModel :: UseCaseModel
}

--emptyInstance :: ProductLine -> FeatureConfiguration -> ProductInstance
--emptyInstance spl fc = 
-- let 
-- 	ucm = splUseCaseModel spl
--	name = ucmName ucm
-- in ProductInstance {
-- 		instanceConfiguration = fc,
-- 		instanceUseCaseModel = UCM { ucmName = name, useCases = [], aspectualUseCases = [] }
-- 	}
 	

type Model2Model = ProductLine -> ProductInstance -> ProductInstance