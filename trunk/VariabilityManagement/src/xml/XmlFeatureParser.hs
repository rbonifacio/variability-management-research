module XmlFeatureParser where

import Text.XML.HXT.Arrow
import System.Environment

import XmlFeatureModel


instance XmlPickler XmlFeature where
	xpickle = xpFeature

xpFeature :: PU XmlFeature
xpFeature =
	xpElem "feature" $
	xpWrap ( uncurry4 XmlFeature, \ (XmlFeature cmin cmax name children) -> (cmin, cmax, name, children) ) $
	xp4Tuple ( xpAttr "min" xpickle ) ( xpAttr "max" xpickle ) ( xpAttr "name" xpText ) ( xpList xpFeature )

