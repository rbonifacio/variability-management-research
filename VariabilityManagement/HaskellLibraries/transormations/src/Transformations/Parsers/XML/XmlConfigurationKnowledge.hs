module Transformations.Parsers.XML.XmlConfigurationKnowledge 
where

import BasicTypes

import ConfigurationKnowledge.Types

import Transformations.ComponentModel
import Transformations.UseCaseModel

import UseCaseModel.Types

import FeatureModel.Parsers.Expression
import FeatureModel.Types

import Text.XML.HXT.Arrow

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

import List
         
data XmlConfigurationKnowledge = XmlConfigurationKnowledge {
      xmlConfigurations :: [XmlConfiguration]
} deriving(Show)  
	
data XmlConfiguration = XmlConfiguration {
      xmlExpression:: String,
      xmlTransformations :: [XmlTransformation]
} deriving(Show)  	
	
data XmlTransformation = XmlTransformation {
      tName :: String,
      tArgs :: String
} deriving(Show)

xml2ConfigurationKnowledge :: XmlConfigurationKnowledge -> ConfigurationKnowledge
xml2ConfigurationKnowledge ck = 
 let cs = xmlConfigurations ck
 in (map xml2Configuration cs) 

xml2Configuration :: XmlConfiguration -> Configuration 
xml2Configuration c =
 let 
  e = parse parseExpression "" (xmlExpression c)
  ts = map xml2Transformation (xmlTransformations c)
 in 
  case e of
   Left err -> error ("Error parsing expression " ++ (xmlExpression c))
   Right exp -> Configuration { expression = exp, transformations = map xml2Transformation (xmlTransformations c) }

xml2Transformation t = 
 let as = splitAndRemoveBlanks ',' (tArgs t)
 in 
  case tName t of 
   "selectScenarios" -> selectScenarios (as)
   
   "bindParameter" -> case as of 
                      [x,y] -> bindParameter x y
                      otherwise -> error "Invalid arguments to the bind parameter function"
   
   "evaluateAspects" -> evaluateAspects (as)
   
   "addTextFragment" -> selectComponents (as)

   otherwise -> error ("Invalid transformation: " ++ tName t)


 	
