\begin{code}
module UseCaseModel.Parsers.XML.XmlConfigurationParser
 where

import Text.XML.HXT.Arrow
import System.Environment

import UseCaseModel.Parsers.XML.XmlConfigurationKnowledge


instance XmlPickler XmlConfigurationKnowledge where
	xpickle = xpConfigurationKnowledge

instance XmlPickler XmlConfiguration where 
	xpickle = xpConfiguration
	
instance XmlPickler XmlTransformation where
 	xpickle = xpTransformation	

xpConfigurationKnowledge :: PU XmlConfigurationKnowledge
xpConfigurationKnowledge =
	xpElem "configurationModel" $
	xpWrap ( uncurry XmlConfigurationKnowledge, \ (XmlConfigurationKnowledge n c) -> (n, c) ) $
	xpPair ( xpAttr "name" xpText ) (xpList xpConfiguration)		 
			 
xpConfiguration :: PU XmlConfiguration
xpConfiguration = 	
	xpElem "configuration" $
	xpWrap ( uncurry XmlConfiguration, \ (XmlConfiguration e f) -> (e, f) ) $
	xpPair ( xpAttr "expression" xpText ) ( xpList xpTransformation)
	
xpTransformation :: PU XmlTransformation
xpTransformation = 	
	xpElem "transformation" $
	xpWrap ( uncurry XmlTransformation, \ (XmlTransformation n a) -> (n, a) ) $
	xpPair ( xpAttr "name" xpText ) ( xpAttr "args" xpText )

\end{code}
	
			 