module XmlFeatureModel where

import BasicTypes

type CMin = Int
type CMax = Int 
type Children = [XmlFeature]
type GroupOptions = Children 


data XmlFeature = XmlFeature CMin CMax Name (Maybe Children) (Maybe XmlGroupFeature)
	deriving(Show)  
	
data XmlGroupFeature = XmlGroupFeature CMin CMax GroupOptions
	deriving(Show)  	