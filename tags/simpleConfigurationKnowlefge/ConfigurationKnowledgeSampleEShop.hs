module ConfigurationKnowledgeSampleEShop where

import ConfigurationKnowledge
import FeatureModel
import UseCaseModel
import FeatureSampleEShop
import UseCaseSampleEShop

exp0 = FeatureRef "FEA-01" -- eShop feature
exp1 = FeatureRef "FEA-14" -- shopping cart
exp2 = FeatureRef "FEA-15" -- register user preferences


-- message feature and not save expression
exp3 = NotExpression exp1 
exp4 = NotExpression exp2

-- Configurations that relate a feature expression with 
-- a set of artifacts (scenarios, in this case)
conf0 = (exp0, [scBuyProductCommon])
conf1 = (exp3, [scBuyProductBasic])
conf2 = (exp1, [scBuyProductExtended])
conf3 = (exp2, [scUpdatePreferences])

ck01 = CK [conf0, conf1, conf2, conf3]


eShop1 = Feature "FEA-01" "eShop Instance" mandatory basicFeature [register01, searchOptions01, paymentType01, shipMethod01] []
eShop2 = Feature "FEA-01" "eShop Instance" mandatory basicFeature [register02, searchOptions02, paymentType02, shipMethod02, shoppingCart, userPreferences] []
-- register configurations
register01 = Feature "FEA-02" "Register type" mandatory alternativeFeature [simple] []
register02 = Feature "FEA-02" "Register type" mandatory alternativeFeature [completely] []

-- search options
searchOptions01 = Feature "FEA-05" "Search options" mandatory orFeature [hints] []
searchOptions02 = Feature "FEA-05" "Search options" mandatory orFeature [hints, similarResults] []

paymentType01 = Feature "FEA-11" "Payment type" mandatory orFeature[invoice] []
paymentType02 = Feature "FEA-11" "Payment type" mandatory orFeature[invoice, credit] []

shipMethod01 = Feature "FEA-16" "Shipping method" mandatory orFeature [economical] []
shipMethod02 = Feature "FEA-16" "Shipping method" mandatory orFeature [economical, fast] []

fc01 = FeatureConfiguration eShop1
fc02 = FeatureConfiguration eShop2