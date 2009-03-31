\begin{code}

module EShopConfigurationKnowledge
where

import EShopFeatureModel
import EShopUseCaseModel

import FeatureModel.Types
import ConfigurationKnowledge.Types
import ConfigurationKnowledge.Interpreter
import UseCaseModel.Types
import UseCaseModel.Transformations

import BasicTypes

c1,c2 :: Configuration

c1 = Configuration (FeatureRef "eShop") [(selectScenarios ["SC01"])]
c2 = Configuration (FeatureRef "ShippingMethod") [(bindParameter "SM" "ShippingMethod")]

eShopCK :: ConfigurationKnowledge

eShopCK = [c1,c2]

iModel01, iModel02 :: InstanceModel

iModel01 = build eShopFM eShopFC01 eShopCK eShopUCM
iModel02 = build eShopFM eShopFC02 eShopCK eShopUCM


\end{code}