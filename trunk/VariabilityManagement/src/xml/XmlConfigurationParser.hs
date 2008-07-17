module XmlConfigurationParser where

import Text.XML.HXT.Arrow
import System.Environment

import XmlConfigurationKnowledge


instance XmlPickler XmlConfigurationKnowledge where
	xpickle = xpConfigurationKnowledge

instance XmlPickler XmlConfiguration where 
	xpickle = xpConfiguration
	
instance XmlPickler XmlFunction where
 	xpickle = xpFunction	

xpConfigurationKnowledge :: PU XmlConfigurationKnowledge
xpConfigurationKnowledge =
	xpElem "configurationModel" $
	xpWrap ( uncurry XmlConfigurationKnowledge, \ (XmlConfigurationKnowledge n c) -> (n, c) ) $
	xpPair ( xpAttr "name" xpText ) (xpList xpConfiguration)		 
			 
xpConfiguration :: PU XmlConfiguration
xpConfiguration = 	
	xpElem "configuration" $
	xpWrap ( uncurry XmlConfiguration, \ (XmlConfiguration e f) -> (e, f) ) $
	xpPair ( xpAttr "expression" xpText ) ( xpList xpFunction )
	
xpFunction :: PU XmlFunction
xpFunction = 	
	xpElem "transformation" $
	xpWrap ( uncurry XmlFunction, \ (XmlFunction n a) -> (n, a) ) $
	xpPair ( xpAttr "expression" xpText ) ( xpAttr "expression" xpText )
	
			 