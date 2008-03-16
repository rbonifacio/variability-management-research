module EshopModelChecking where 

import ShowFunctions
import QuickCheck

import BasicTypes
import FeatureModel

instance Arbitrary FeatureModel where 
 arbitrary = do frequency [(5, featureModelGen)] 
 
instance Arbitrary FeatureConfiguration where 
 arbitrary = do frequency [(5, featureConfigurationGen)]
  
instance Arbitrary Feature where
    arbitrary = do
        frequency [(5, featureGenNormal),  (0, return FeatureError) ]
 
letterGen = oneof $ map return $ ['a'..'z'] ++ ['A'..'Z']

listGen :: Gen a -> Gen [a]
listGen g = do
    x <- g
    xs <- frequency [ (1, return []), (10, listGen g) ]
    return (x:xs)

stringGen :: Gen String
stringGen = listGen letterGen


featureModelGen = do 
    root <- featureGenNormal
    return (FeatureModel root)
    
featureConfigurationGen = do 
    root <- featureGenNormal
    return (FeatureConfiguration root)

featureGenNormal = do
    id <- stringGen
    name <- stringGen
    featuretype <- arbitrary
    grouptype <- arbitrary
    children <- arbitrary
    --properties <-  [["abc", "abc"]]
    return (Feature id name featuretype grouptype children []) 
    
prop_RootFeature :: FeatureModel -> FeatureConfiguration -> QuickCheck.Property
prop_RootFeature fm fc = not (fmRoot fm == fcRoot fc) ==> length (validInstance fm fc) == 1


prop_MandatoryFeature :: Feature -> Feature -> QuickCheck.Property 
prop_MandatoryFeature fm fc = 
 (not (and [featureExists (children fc) (fId x) | x <- children fm, fType x == mandatory])) ==> length (checkBasicFeature fm fc) > 1 
    
    
prop_AlternativeFeature :: Feature -> Feature -> QuickCheck.Property 
prop_AlternativeFeature fm fc = length (children fc) == 0 ==> length (checkAlternativeFeature fm fc) > 0 
-- where  types = (fm :: Feature, fc :: Feature) 