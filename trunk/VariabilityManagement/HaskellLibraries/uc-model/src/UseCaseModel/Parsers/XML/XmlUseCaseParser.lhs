\begin{code}
module UseCaseModel.Parsers.XML.XmlUseCaseParser where

import Text.XML.HXT.Arrow
import System.Environment

import UseCaseModel.Parsers.XML.XmlUseCaseModel


instance XmlPickler XmlUseCaseModel where
	xpickle = xpUseCaseModel

instance XmlPickler XmlUseCase where
	xpickle = xpUseCase
	
instance XmlPickler XmlAspectualUseCase where 
	xpickle = xpAspectualUseCase	

instance XmlPickler XmlAdvice where 
	xpickle = xpAdvice
		
instance XmlPickler XmlScenario where 
 	xpickle = xpScenario

instance XmlPickler XmlStep where 
	xpickle = xpStep
	

-- 
-- necessary, if the element has more than five sub-elements or attributes.	
--
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

xpUseCaseModel :: PU XmlUseCaseModel
xpUseCaseModel =
	xpElem "useCaseModel" $
	xpWrap ( uncurry3 XmlUCM, \ (XmlUCM n ucs aspect) -> (n, ucs, aspect) ) $
	xpTriple ( xpAttr "name" xpText ) ( xpList xpUseCase )  ( xpList xpAspectualUseCase )

xpUseCase :: PU XmlUseCase
xpUseCase =
	xpElem "useCase" $
	xpWrap ( uncurry4 XmlUseCase, \ (XmlUseCase i n d s) -> (i, n, d, s) ) $
	xp4Tuple (xpElem "id" xpText) (xpElem "name" xpText) (xpElem "description" xpText) (xpList xpScenario)

xpAspectualUseCase :: PU XmlAspectualUseCase
xpAspectualUseCase = 
	xpElem "aspect"	$
	xpWrap ( uncurry3 XmlAspectualUseCase, \ (XmlAspectualUseCase i n a) -> (i, n,a) ) $
	xpTriple (xpElem "id" xpText) (xpElem "name" xpText) (xpList xpAdvice)	

xpAdvice :: PU XmlAdvice
xpAdvice = 
	xpElem "advice" $
	xpWrap ( uncurry3 XmlAdvice, \ (XmlAdvice t p a) -> (t, p, a) )	$
	xpTriple (xpAttr "type" xpText) (xpElem "pointcut" xpText) (xpScenario)

--
-- In the current UseCase xml document, scenarios have no id.
-- This may be change in the future
---	
xpScenario :: PU XmlScenario
xpScenario = 
	xpElem "flow" $
	xpWrap ( uncurry5 XmlScenario, \ (XmlScenario i d f t s) -> (i, d, f, t, s) ) $
	xp5Tuple (xpElem "id" xpText )
			 (xpElem "description" xpText ) 
			 (xpElem "fromSteps" xpText) 
			 (xpElem "toSteps" xpText) 
			 (xpList xpStep) 

xpStep :: PU XmlStep 
xpStep = 
	xpElem "step" $
	xpWrap ( uncurry4 XmlStep, \ (XmlStep i a  s r) -> (i, a, s, r) ) $
	xp4Tuple (xpElem "stepId" xpText) (xpElem "action" xpText ) (xpElem "condition" xpText) (xpElem "response" xpText)
\end{code}
	

