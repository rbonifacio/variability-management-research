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

cn1,cn2,cn3,cn4,cn5 :: Configuration

cn1 = Configuration (FeatureRef "eShop") [(selectScenarios ["SC01","SC02"])]
cn2 = Configuration (FeatureRef "ShippingMethod") [(bindParameter "SM" "ShippingMethod")]
cn3 = Configuration (Not (FeatureRef "ShoppingCart")) [(evaluateAspect ["ADV01"])]
cn4 = Configuration (FeatureRef "ShoppingCar") [(evaluateAspect ["ADV02"])]
cn5 = Configuration (FeatureRef "UpdatePreferences") [(evaluateAspect ["ADV03"])]

eShopCK :: ConfigurationKnowledge
eShopCK = [cn1,cn2,cn3,cn4,cn5]

iModel01, iModel02 :: InstanceModel

iModel01 = build eShopFM eShopFC01 eShopCK eShopUCM
iModel02 = build eShopFM eShopFC02 eShopCK eShopUCM


\end{code}