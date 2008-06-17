module ShopMain where

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

expectedTracesC1 = [[], 
 				   ["start"], 
 				   ["start", "1M"], 
 				   ["start", "1M", "2M"],
 				   ["start","1M", "2M", "3M"], 
 				   ["start","1M", "2M", "3M", "4M", "5M", "end"]]

nonExpectedTrace = [["start", "V1"], ["start", "V1", "V3"]]

tm = traceModelWeaver fm01 fc01 ck01 ucm01 env01

testExpectedTrace = TestCase (assertBool "Expected trace" (traceRefinement expectedTracesC1 tm))

tests = TestList [TestLabel "Expected trace" testExpectedTrace]


-- test1 = TestCase (assertEqual )



