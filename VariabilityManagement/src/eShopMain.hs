module EShopMain where

import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment
import FeatureSampleEShop
import UseCaseSampleEShop
import ConfigurationKnowledgeSampleEShop

env1 = Environment[EnvItem ("ShipMethod", shipMethod01), EnvItem ("MaxMsg", fcSize1)]
env2 = Environment[EnvItem ("MessageType", shipMethod02), EnvItem ("MaxMsg", fcSize2)] 