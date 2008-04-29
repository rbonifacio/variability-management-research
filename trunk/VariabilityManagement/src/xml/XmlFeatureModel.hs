module XmlFeatureModel where

import BasicTypes

type CMin = Int
type CMax = Int 
type Children = [XmlFeature]


data XmlFeature = XmlFeature CMin CMax Name Children 
	deriving(Show)  