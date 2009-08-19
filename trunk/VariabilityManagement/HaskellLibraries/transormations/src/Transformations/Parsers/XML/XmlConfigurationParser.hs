-----------------------------------------------------------------------------
-- |
-- Module      :  Transformations.Parsers.XML.XmlConfigurationParser
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- A XML parser for our configuration model.
--
-----------------------------------------------------------------------------
module Transformations.Parsers.XML.XmlConfigurationParser(parseConfigurationKnowledge)
where

import BasicTypes 

import Text.XML.HXT.Arrow
import System.Environment

import Transformations.Parsers.XML.XmlConfigurationKnowledge

-- | 
-- Parser for a configuration knowledge file. 
-- It results either Success or Fail, if the file is 
-- not valid.
--
parseConfigurationKnowledge fileName = 
 do
   c <- runX ( xunpickleDocument xpConfigurationKnowledge [ (a_validate,v_0)
 				                          , (a_trace, v_1)
 				                          , (a_remove_whitespace,v_1)
 				                          , (a_preserve_comment, v_0)
                                                          ] (createURI fileName) )
   case c of 
     [x] -> return $ Success (xml2ConfigurationKnowledge x)
     otherwise -> return $ Fail "Error parsing the configuration knowledge file. Try to check the input file."
   
-- 
-- The parser implementation using HXT library.
-- It requires several picklers.
-- 
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


	
			 