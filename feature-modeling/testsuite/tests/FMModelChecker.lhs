\begin{code}
module FMModelChecker where 

-- import ShowFunctions
import Test.QuickCheck

import FeatureModel.Types
import FeatureModel.FMTypeChecker
import FeatureModel.FCTypeChecker

instance Arbitrary FeatureModel where 
 arbitrary = do frequency [(5, featureModelGen)] 
 
instance Arbitrary FeatureConfiguration where 
 arbitrary = do frequency [(5, featureConfigurationGen)]
  
instance Arbitrary Feature where
    arbitrary = do
        frequency [(5, featureGenNormal),  (0, return FeatureError) ]
 
letterGen = oneof $ map return $ ['a'..'z'] ++ ['A'..'Z']
idGen = oneof [return "01", return "02", return "03", return "04"]

listGen :: Gen a -> Gen [a]
listGen g = do
    x <- g
    xs <- frequency [ (1, return []), (10, listGen g) ]
    return (x:xs)

stringGen :: Gen String
stringGen = listGen letterGen

featureTypeGen :: Gen FeatureType
featureTypeGen = oneof [return Optional, return Mandatory]

groupTypeGen :: Gen GroupType
groupTypeGen = oneof [return OrFeature, return AlternativeFeature, return BasicFeature]

featureModelGen = do 
    root <- featureGenNormal
    return (FeatureModel root [])
    
featureConfigurationGen = do 
    root <- featureGenNormal
    return (FeatureConfiguration root)

featureGenNormal = do
    fid <- idGen
    name <- stringGen
    featuretype <- featureTypeGen
    grouptype <- groupTypeGen
    children <- arbitrary
    return (Feature fid name featuretype grouptype children []) 

prop_RootFeature :: FeatureModel -> FeatureConfiguration -> Test.QuickCheck.Property
prop_RootFeature fm fc = not (fmRoot fm == fcRoot fc) ==> 
 existError (validInstance fm fc) 

prop_FMTypeChecker :: FeatureModel -> FeatureConfiguration -> Test.QuickCheck.Property
prop_FMTypeChecker fm fc = not ((fmTypeChecker fm) == Success) ==> 
 existError (validInstance fm fc)

prop_ValidInstance :: FeatureModel -> FeatureConfiguration -> Test.QuickCheck.Property
prop_ValidInstance fm fc = not (existError (validInstance fm fc)) ==> 
 validInstance' fm fc 

\end{code}