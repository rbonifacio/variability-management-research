module XmlUseCaseParser where

import Text.XML.HXT.Arrow
import System.Environment

import XmlUseCaseModel


instance XmlPickler XmlUseCaseModel where
	xpickle = xpUseCaseModel

instance XmlPickler XmlUseCase where
	xpickle = xpUseCase

instance XmlPickler XmlScenario where 
 	xpickle = xpScenario

instance XmlPickler XmlStep where 
	xpickle = xpStep
	

-- 
-- necessary, if the element has more five sub-elements or attributes.	
--
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

xpUseCaseModel :: PU XmlUseCaseModel
xpUseCaseModel =
	xpElem "useCaseModel" $
	xpWrap ( uncurry XmlUCM, \ (XmlUCM n ucs) -> (n, ucs) ) $
	xpPair ( xpAttr "name" xpText ) ( xpList xpUseCase )

xpUseCase :: PU XmlUseCase
xpUseCase =
	xpElem "useCase" $
	xpWrap ( uncurry4 XmlUseCase, \ (XmlUseCase i n d s) -> (i, n, d, s) ) $
	xp4Tuple (xpElem "id" xpText) (xpElem "name" xpText) (xpElem "description" xpText) (xpList xpScenario)

--
-- In the current UseCase xml document, scenarios have no id.
-- This may be change in the future
---	
xpScenario :: PU XmlScenario
xpScenario = 
	xpElem "flow" $
	xpWrap ( uncurry4 XmlScenario, \ (XmlScenario d f t s) -> (d, f, t, s) ) $
	xp4Tuple (xpElem "description" xpText ) (xpElem "fromSteps" xpText) (xpElem "toSteps" xpText) (xpList xpStep) 

xpStep :: PU XmlStep 
xpStep = 
	xpElem "step" $
	xpWrap ( uncurry4 XmlStep, \ (XmlStep i a  s r) -> (i, a, s, r) ) $
	xp4Tuple (xpElem "stepId" xpText) (xpElem "action" xpText ) (xpElem "condition" xpText) (xpElem "response" xpText)
	

