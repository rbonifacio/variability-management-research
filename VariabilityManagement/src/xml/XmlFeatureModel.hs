module XmlFeatureModel where

import BasicTypes
-- import FeatureModel

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
	
data XmlGroupFeature = XmlGroupFeature CMin CMax XmlGroupOptions
	deriving(Show)  	
	
