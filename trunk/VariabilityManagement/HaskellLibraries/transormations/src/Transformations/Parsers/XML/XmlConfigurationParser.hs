module Transformations.Parsers.XML.XmlConfigurationParser
 where

import Text.XML.HXT.Arrow
import System.Environment

import Transformations.Parsers.XML.XmlConfigurationKnowledge

parseConfigurationKnowledge fileName = 
 do
   [x] <- runX ( xunpickleDocument xpConfigurationKnowledge [ (a_validate,v_0)
 				                            , (a_trace, v_1)
 				                            , (a_remove_whitespace,v_1)
 				                            , (a_preserve_comment, v_0)
 				                            ] fileName )
   let ck = xml2ConfigurationKnowledge x
   return ck

instance XmlPickler XmlConfigurationKnowledge where
	xpickle = xpConfigurationKnowledge

instance XmlPickler XmlConfiguration where 
	xpickle = xpConfiguration
	
instance XmlPickler XmlTransformation where
 	xpickle = xpTransformation	

xpConfigurationKnowledge :: PU XmlConfigurationKnowledge
xpConfigurationKnowledge =
	xpElem "configurationModel" $
	xpWrap ( XmlConfigurationKnowledge, \ (XmlConfigurationKnowledge c) -> (c) ) $
        (xpList xpConfiguration)		 
			 
xpConfiguration :: PU XmlConfiguration
xpConfiguration = 	
	xpElem "configuration" $
	xpWrap ( uncurry XmlConfiguration, \ (XmlConfiguration e t) -> (e, t) ) $
	xpPair ( xpElem "expression" xpText ) ( xpList xpTransformation)
	
xpTransformation :: PU XmlTransformation
xpTransformation = 	
	xpElem "transformation" $
	xpWrap ( uncurry XmlTransformation, \ (XmlTransformation n a) -> (n, a) ) $
	xpPair ( xpElem "name" xpText ) ( xpElem "args" xpText )


	
			 