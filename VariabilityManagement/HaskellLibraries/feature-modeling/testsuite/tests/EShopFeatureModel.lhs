\begin{code}

module EShopFeatureModel where 

import FeatureModel.Types
import FeatureModel.FMTypeChecker
import FeatureModel.FCTypeChecker

economical, fast, foreignShip :: FeatureTree

hints, similarResults :: FeatureTree

economical = createOption "Economical"
fast = createOption "Fast"
foreignShip = createOption "ForeignShip" 

hints = createOption "Hints"
similarResults = createOption "SimilarResults" 

shoppingCart, bonus, updatePreferences :: FeatureTree

shoppingCart = createFeatureTree "ShoppingCart" Optional BasicFeature []
bonus = createFeatureTree "Bonus" Optional BasicFeature []
updatePreferences = createFeatureTree "UpdatePreferences" Optional BasicFeature []

shippingMethod, searchOptions :: FeatureTree

shippingMethod = createFeatureTree "ShippingMethod" Mandatory OrFeature [economical, fast, foreignShip]

searchOptions = createFeatureTree "SearchOptions" Mandatory AlternativeFeature [hints, similarResults]

eShop :: FeatureTree

eShop = createFeatureTree "eShop" 
                           Mandatory 
                           BasicFeature 
                           [shoppingCart, bonus, updatePreferences, shippingMethod, searchOptions]

eShopFM :: FeatureModel

eShopFM = FeatureModel eShop []

-- configuration 01
fcSM01, fcSO01, fcEshop01 :: FeatureTree

fcSM01 = createFeatureTree "ShippingMethod" Mandatory OrFeature [economical, fast]
fcSO01 = createFeatureTree "SearchOptions" Mandatory AlternativeFeature [hints]
fcEshop01 = createFeatureTree "eShop" Mandatory BasicFeature [fcSM01, fcSO01]

eShopFC01 :: FeatureConfiguration
eShopFC01 = FeatureConfiguration fcEshop01

-- configuration 02
fcSM02, fcSO02, fcEshop02 :: FeatureTree

fcSM02 = createFeatureTree "ShippingMethod" Mandatory OrFeature [economical]
fcSO02 = createFeatureTree "SearchOptions" Mandatory AlternativeFeature [similarResults]
fcEshop02 = createFeatureTree "eShop" 
                              Mandatory 
                              BasicFeature 
                              [shoppingCart, bonus, updatePreferences, fcSM02, fcSO02]

eShopFC02 :: FeatureConfiguration
eShopFC02 = FeatureConfiguration fcEshop02




createFeatureTree :: String -> FeatureType -> GroupType -> [FeatureTree] -> FeatureTree
createFeatureTree i t g c = 
 case c of 
  [] -> Leaf (Feature i i t g [])
  otherwise -> Root (Feature i i t g []) c

createOption :: String -> FeatureTree
createOption fid  = 
 Leaf (Feature fid fid Optional BasicFeature [])

checkEShop :: CheckerResult
checkEShop = fmTypeChecker eShopFM

checkFC01 :: Bool
checkFC02 :: Bool

checkFC01 = validInstance'  eShopFM eShopFC01
checkFC02 = validInstance'  eShopFM eShopFC02

\end{code}