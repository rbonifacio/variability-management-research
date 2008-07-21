module XmlConfigurationKnowledge where

import BasicTypes
import AbstractModel
import ConfigurationKnowledge
import UseCaseModel2Model
import UseCaseModel
import List

data XmlConfigurationKnowledge = XmlConfigurationKnowledge {
		cName :: String,
		xmlConfigurations :: [XmlConfiguration]
	}
	deriving(Show)  
	
data XmlConfiguration = XmlConfiguration {
		xmlExpression:: String,
		xmlFunction :: [XmlFunction]
	}
	deriving(Show)  	
	
data XmlFunction = XmlFunction {
 		fName :: String,
 		fArgs :: String
 	}	
 	deriving(Show)
 	
--xmlConfigurationKnowledge2ConfigurationKnowledge :: XmlConfigurationKnowledge -> ConfigurationKnowledge UseCaseModel
--xmlConfigurationKnowledge2ConfigurationKnowledge xmlCK = 
-- ConfigurationKnowledge xmlConfiguration2Configuration (xmlConfigurations xmlCK) 	
--
--xmlConfiguration2Configuration :: [XmlConfiguratio] -> [Configuration UseCaseModel]
--xmlConfiguration2Configuration [] = []
--xmlConfiguration2Configuration (x:xs) = 
-- Configuration UseCaseModel { 
-- 	expression = xmlExpression x, 
-- 	transformations = (xmlFunction2Function xmlFunction x) 
-- } : (xmlConfiguration2Configuration xs) 

xmlFunction2Function :: [XmlFunction] -> [Model2Model UseCaseModel]
xmlFunction2Function [] = []
xmlFunction2Function (x:xs) = 
 case fName x of
 	"addScenarios"   -> let ids = split ',' (fArgs x) 
 					    in ConsM2MType1 (addScenariosM2M ids) : xmlFunction2Function (xs)
 					    
 	"bindParameters" -> let args = split ',' (fArgs x)
 						in if length args == 2 
 						 	then ConsM2MType2 (bindParametersM2M (args !! 0) (args !! 1)) : xmlFunction2Function (xs)
 						 	else error "error... "				    
 	
-- 	"evaluateAspect" -> let aspect = fArgs x  	
-- 						in ConsM2MType0 evaluateAspectM2M aspect

--transformation2XmlFunction :: [Model2Model UseCaseModel] -> [XmlFunction]
--transformation2XmlFunction [] = []
--transformation2XmlFunction (x:xs) = 
-- case x of 
--  ConsM2MType1 a -> 
--   case a of 
--    addScenariosM2M  ->  XmlFunction { fName = "addScenarios", fArgs = (replaceElement ' ' ',' (unwords ids))}