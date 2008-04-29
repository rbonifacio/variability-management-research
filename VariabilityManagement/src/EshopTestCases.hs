module EshopTestCases where

import Weaver
import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment
import FeatureSampleEShop
import UseCaseSampleEShop
import ConfigurationKnowledgeSampleEShop
import HUnit

env01 = Environment[EnvItem ("ShipMethod", shipMethod01)]
env02 = Environment[EnvItem ("ShipMethod", shipMethod02)] 

-- expected traces for the first configuration
tcData01 = [[], 
 		   ["start"], 
 		   ["start", "1M"], 
 		   ["start", "1M", "2M"],
 		   ["start","1M", "2M", "3M"], 
 		   ["start","1M", "2M", "3M", "4M", "5M", "end"]]

-- expected traces for the second configuration
tcData02 = [[],
			["start"],	
			["start", "V1"], 
			["start", "V1", "V2"],
			["start", "V1", "V2", "3M"],
			["start", "V1", "V2", "3M", "4M"],
			["start", "V1", "V2", "3M", "4M", "5M"],
			["start", "V1", "V2", "3M", "4M", "5M","end"]]
			
			

-- trace model for the first configuration
tm01 = traceModelWeaver fm01 fc01 ck01 ucm01 env01

-- trace model for the second configuration
tm02 = traceModelWeaver fm01 fc02 ck01 ucm01 env02

-- test expected traces for configuration 01
tc01 = TestCase (assertBool "tcData01 should be refined by tm01" (traceRefinement tcData01 tm01))

-- test non-expected traces for configuration 01
tc02 = TestCase (assertBool "tcData01 should not be refined by tm01" (not (traceRefinement tcData02 tm01)))

-- test expected traces for configuration 01
tc03 = TestCase (assertBool "tcData02 should be refined by tm02" (traceRefinement tcData02 tm02))

-- test non-expected traces for configuration 01
tc04 = TestCase (assertBool "tcData02 should not be refined by tm02" (not (traceRefinement tcData01 tm02)))



tests = TestList [TestLabel "TC01" tc01, TestLabel "TC02" tc02, TestLabel "TC03" tc03, TestLabel "TC04" tc04]


-- test1 = TestCase (assertEqual )



