\begin{code}

module FeatureModel.Parsers.FMPlugin.XmlFeatureModel where

import FeatureModel.Types

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
	 then Optional 
	 else Mandatory
	 
groupTypeFromXmlGroup :: (Maybe XmlGroupFeature) -> GroupType
groupTypeFromXmlGroup Nothing = BasicFeature
groupTypeFromXmlGroup (Just (XmlGroupFeature cmin cmax options) )= 
	if (cmin == 1)
	 then AlternativeFeature
	 else OrFeature
	
childrenFromXmlFeatureList :: Maybe XmlChildren -> Maybe XmlGroupFeature -> Children
childrenFromXmlFeatureList _  (Just (XmlGroupFeature _ _ options)) = [xmlFeature2Feature x | x <- options] 
childrenFromXmlFeatureList (Just (children))  Nothing = [xmlFeature2Feature x | x <- children] 
	 
\end{code}	 
 	 