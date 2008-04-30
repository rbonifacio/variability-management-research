module XmlFeatureParser where

import Text.XML.HXT.Arrow
import System.Environment

import XmlFeatureModel


instance XmlPickler XmlFeature where
	xpickle = xpFeature

instance XmlPickler XmlGroupFeature where 
	xpickle = xpGroup 
	
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

xpFeature :: PU XmlFeature
xpFeature =
	xpElem "feature" $
	xpWrap ( uncurry5 XmlFeature, \ (XmlFeature cmin cmax name children group) -> (cmin, cmax, name, children, group) ) $
	xp5Tuple ( xpAttr "min" xpickle ) 
			 ( xpAttr "max" xpickle ) 
			 ( xpAttr "name" xpText ) 
			 (xpOption (xpList xpFeature) )
			 (xpOption (xpGroup) )
			 
xpGroup :: PU XmlGroupFeature
xpGroup = 	
	xpElem "featureGroup" $
	xpWrap ( uncurry3 XmlGroupFeature, \ (XmlGroupFeature cmin cmax options) -> (cmin, cmax, options) ) $
	xpTriple ( xpAttr "min" xpickle ) 
			 ( xpAttr "max" xpickle ) 
			 (xpList xpFeature)
			 