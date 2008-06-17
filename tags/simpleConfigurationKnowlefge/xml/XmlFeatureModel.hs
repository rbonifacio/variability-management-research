module XmlFeatureModel where

import BasicTypes
import FeatureModel

type CMin = Int
type CMax = Int 
type XmlChildren = [XmlFeature]
type XmlGroupOptions = XmlChildren 


data XmlFeature = XmlFeature {
		featureId :: Id,
		cmin :: CMin,
		cmax :: CMax, 
		name :: Name, 
		children :: Maybe XmlChildren,
		group :: Maybe XmlGroupFeature 
	}
	deriving(Show)  
	
data XmlGroupFeature = XmlGroupFeature {
		gmin :: CMin,
		gmax :: CMax, 
		options :: XmlGroupOptions
	}
	deriving(Show)  	
	
xmlFeature2Feature :: XmlFeature -> Feature	
xmlFeature2Feature (XmlFeature fid cmin cmax name children group) = 
 Feature fid 
 		 name 
 		 (featureTypeFromCardinality cmin)
 		 (groupTypeFromXmlGroup group)
 		 (childrenFromXmlFeatureList children group)
 		 []
 		 

featureTypeFromCardinality :: CMin -> FeatureType	
featureTypeFromCardinality cmin = 
	if (cmin == 0) 
	 then optional 
	 else mandatory
	 
groupTypeFromXmlGroup :: (Maybe XmlGroupFeature) -> GroupType
groupTypeFromXmlGroup Nothing = basicFeature
groupTypeFromXmlGroup (Just (XmlGroupFeature cmin cmax options) )= 
	if (cmin == 1)
	 then alternativeFeature
	 else orFeature
	
childrenFromXmlFeatureList :: Maybe XmlChildren -> Maybe XmlGroupFeature -> Children
childrenFromXmlFeatureList _  (Just (XmlGroupFeature _ _ options)) = [xmlFeature2Feature x | x <- options] 
childrenFromXmlFeatureList (Just (children))  Nothing = [xmlFeature2Feature x | x <- children] 
	 
	 
 	 