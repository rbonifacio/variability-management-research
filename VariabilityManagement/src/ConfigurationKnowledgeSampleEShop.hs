module ConfigurationKnowledgeSampleEShop where

import AbstractModel
import ConfigurationKnowledge
import FeatureModel
import UseCaseModel
import UseCaseModel2Model
import FeatureSampleEShop
import UseCaseSampleEShop

exp0 = FeatureRef "FEA-01" -- eShop feature
exp1 = FeatureRef "FEA-14" -- shopping cart
exp2 = FeatureRef "FEA-15" -- register user preferences


-- message feature and not save expression
exp3 = NotExpression exp1 
exp4 = NotExpression exp2


-- Configurations that relate a feature expression with 
-- a set of model2model functions
conf0 = Configuration exp0 [(ConsM2MType1 (addScenariosM2M ["1", "4"]))]
--conf1 = Configuration exp3 [(addScenariosM2M ["1"])]
conf2 = Configuration exp1 [(ConsM2MType1 (addScenariosM2M ["2"]))]
conf3 = Configuration exp2 [(ConsM2MType1 (addScenariosM2M ["3"]))]

ck01 = [conf0,conf2, conf3]


eShop1 = Feature "FEA-01" "eShop Instance" mandatory basicFeature [register01, searchOptions01, paymentType01, shipMethod01] []
eShop2 = Feature "FEA-01" "eShop Instance" mandatory basicFeature [register02, searchOptions02, paymentType02, shipMethod02, shoppingCart, userPreferences] []

---- register configurations
register01 = Feature "FEA-02" "Register type" mandatory alternativeFeature [simple] []
register02 = Feature "FEA-02" "Register type" mandatory alternativeFeature [completely] []
--
---- search options
searchOptions01 = Feature "FEA-05" "Search options" mandatory orFeature [hints] []
searchOptions02 = Feature "FEA-05" "Search options" mandatory orFeature [hints, similarResults] []
--
paymentType01 = Feature "FEA-11" "Payment type" mandatory orFeature[invoice] []
paymentType02 = Feature "FEA-11" "Payment type" mandatory orFeature[invoice, credit] []
--
shipMethod01 = Feature "FEA-16" "Shipping method" mandatory orFeature [economical] []
shipMethod02 = Feature "FEA-16" "Shipping method" mandatory orFeature [economical, fast] []
--
fc01 = FeatureConfiguration eShop1
fc02 = FeatureConfiguration eShop2