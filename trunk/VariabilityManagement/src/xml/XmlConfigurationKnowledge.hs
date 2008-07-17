module XmlConfigurationKnowledge where

--import BasicTypes

data XmlConfigurationKnowledge = XmlConfigurationKnowledge {
		cName :: String,
		configurations :: [XmlConfiguration]
	}
	deriving(Show)  
	
data XmlConfiguration = XmlConfiguration {
		expression:: String,
		function :: [XmlFunction]
	}
	deriving(Show)  	
	
data XmlFunction = XmlFunction {
 		fName :: String,
 		fArgs :: String
 	}	
 	deriving(Show)
	