module FeatureSampleEShop where

import FeatureModel
 
fm01 = FeatureModel eshop

eshop = Feature "FEA-01" "eShop Product Line" mandatory basicFeature [register, searchOptions, bonusOptions, paymentType, shipMethod, shoppingCart, userPreferences] []

-- Register type feature
register = Feature "FEA-02" "Register type" mandatory alternativeFeature [completely, simple] []
completely = Feature "FEA-03" "Completely register" optional basicFeature [] []
simple = Feature "FEA-04" "Simple register" optional basicFeature [] []

-- Search options feature
searchOptions = Feature "FEA-05" "Search options" mandatory orFeature [hints, similarResults] []
hints = Feature "FEA-06" "Hint based search" optional basicFeature [] []
similarResults = Feature "FEA-07" "Similare results based search" optional basicFeature [] []

-- Types of bonus feature
bonusOptions = Feature "FEA-08" "Bonus" optional orFeature [points, deduction] []
points = Feature "FEA-09" "Points based bonus" optional basicFeature [] []
deduction = Feature "FEA-10" "Deduction based bonus" optional basicFeature [] []

paymentType = Feature "FEA-11" "Payment type" mandatory orFeature[invoice, credit] []
invoice = Feature "FEA-12" "Invoice payment type" optional basicFeature [][] 
credit = Feature "FEA-13" "Credit-card payment type" optional basicFeature [] [] 

shoppingCart = Feature "FEA-14" "Shopping cart capability" optional basicFeature [][] 

userPreferences = Feature "FEA-15" "Register user prefernces" optional basicFeature [] [] 

shipMethod = Feature "FEA-16" "Shipping method" mandatory orFeature [economical, fast] []
economical = Feature "FEA-17" "Economical shipping method" optional basicFeature [][] 
fast = Feature "FEA-18" "Fast shipping method" optional basicFeature [][] 

