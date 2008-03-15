
module eShopMain where

import TraceModel
import UseCaseModel
import FeatureModel
import ConfigurationKnowledge
import Environment
import FeatureSampleEShop
import UseCaseSampleEShop
import ConfigurationKnowledgeSampleEShop

env1 = Environment[EnvItem ("MessageType", fcAllMessageType), EnvItem ("MaxMsg", fcSize1)]
env2 = Environment[EnvItem ("MessageType", fcNotAllMessageType), EnvItem ("MaxMsg", fcSize2)] 