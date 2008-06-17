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
idGen = oneof [return "01", return "02", return "03", return "04"]
--idGen = oneof $ map return $ ['1'..'9']

listGen :: Gen a -> Gen [a]
listGen g = do
    x <- g
    xs <- frequency [ (1, return []), (10, listGen g) ]
    return (x:xs)

stringGen :: Gen String
stringGen = listGen letterGen

featureTypeGen :: Gen Integer
featureTypeGen = oneof [return optional, return mandatory]

groupTypeGen :: Gen Integer
groupTypeGen = oneof [return orFeature, return alternativeFeature, return basicFeature]

featureModelGen = do 
    root <- featureGenNormal
    return (FeatureModel root)
    
featureConfigurationGen = do 
    root <- featureGenNormal
    return (FeatureConfiguration root)

featureGenNormal = do
    id <- idGen
    name <- stringGen
    featuretype <- featureTypeGen
    grouptype <- groupTypeGen
    children <- arbitrary
    return (Feature id name featuretype grouptype children []) 
    
prop_RootFeature :: FeatureModel -> FeatureConfiguration -> QuickCheck.Property
prop_RootFeature fm fc = not (fmRoot fm == fcRoot fc) ==> 
 existError (validInstance fm fc) 
    
prop_AlternativeFeatureOneSelected :: Feature -> Feature -> QuickCheck.Property 
prop_AlternativeFeatureOneSelected fm fc = (not (length (children fc) == 1)) ==> 
 existError (checkAlternativeFeature fm fc) 

prop_AlternativeFeatureExist :: Feature -> Feature -> QuickCheck.Property
prop_AlternativeFeatureExist fm fc = 
 not (allFeatureExists (children(fm)) (children(fc))) ==> 
 existError (checkAlternativeFeature fm fc) 

prop_OrFeatureAtLeastOne :: Feature -> Feature -> QuickCheck.Property
prop_OrFeatureAtLeastOne fm fc = and [length (children fc) == 0, length (children fm) > 0] ==> 
 collect (length (children fm))$
 existError (checkOrFeature fm fc) 
 
prop_OrFeature :: Feature -> Feature -> QuickCheck.Property
prop_OrFeature fm fc = 
 and [not (allFeatureExists (children fm) (children fc)), length (children fm) > 0] ==>
 existError (checkOrFeature fm fc) 
  

