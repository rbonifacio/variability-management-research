module EshopModelChecking where 

import ShowFunctions
import QuickCheck

import BasicTypes
import FeatureModel

-- Id Name FeatureType GroupType Children Properties

data Colour = Red | Green | Blue

instance Arbitrary Colour where 
 arbitrary = oneof [return Red, return Green, return Blue]

--instance Arbitrary Feature where 
-- arbitrary = oneof [return (Feature "a" "a" 0 0 []  []) ]


parAlt01 = Feature "01" "ParAlt01" mandatory alternativeFeature [] []
 
instance Arbitrary Feature where 
 arbitrary = oneof [return (parAlt01), 
 					return (Feature "01" "PatAlt01" mandatory alternativeFeature [] [] ), 
 					return (Feature "02" "ChdAlt01" optional basicFeature [] [])]
 
prop_AlternativeFeature :: Feature -> Feature -> QuickCheck.Property 
prop_AlternativeFeature fm fc = length (children fc) == 0 ==> length (checkAlternativeFeature fm fc) > 0 
-- where  types = (fm :: Feature, fc :: Feature) 