-----------------------------------------------------------------------------
-- |
-- Module      :  UseCaseModel.Parsers.XML.XmlUseCaseParser
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- This module defines several functions for parsing 
-- a TaRGeT xml document. We develped this module using the HXT (Haskell XML 
-- Toolkit) library. 
--
-----------------------------------------------------------------------------
module UseCaseModel.Parsers.XML.XmlUseCaseParser where

import Text.XML.HXT.Arrow
import System.Environment

import UseCaseModel.Parsers.XML.XmlUseCaseModel

parseUseCaseModel fileName = 
 do
   [x] <- runX ( xunpickleDocument xpUseCaseModel [ (a_validate,v_0)
 					          , (a_trace, v_1)
 					          , (a_remove_whitespace,v_1)
 					          , (a_preserve_comment, v_0)
 					          ] fileName )
   let ucmodel = xmlUseCaseModel2UseCaseModel x
   return ucmodel  

--
-- based on the HXT library, we have to declare one instance of 
-- XmlPickler for each element of our data structures. 
-- 

instance XmlPickler XmlPhone where 
         xpickle = xpPhone

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

-- instance XmlPickler XmlAdviceFlow where 
--        xpickle = xpAdviceFlow 

instance XmlPickler XmlStep where 
	xpickle = xpStep
-- 
-- necessary, if the element has more than five sub-elements or attributes.	
--
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

xpPhone :: PU XmlPhone
xpPhone = 
        xpElem "phone" $
        xpWrap (XmlPhone, \ (XmlPhone ucms) -> (ucms) ) $ 
        xpList (xpUseCaseModel)

xpUseCaseModel :: PU XmlUseCaseModel
xpUseCaseModel =
	xpElem "feature" $ -- it should be useCaseModel, but we change to TaRGeT format.
	xpWrap ( uncurry4 XmlUCM, \ (XmlUCM i n ucs aspect) -> (i, n, ucs, aspect) ) $
	xp4Tuple ( xpElem "id" xpText ) 
                 ( xpElem "name" xpText ) 
                 ( xpList xpUseCase )  
                 ( xpList xpAspectualUseCase )

xpUseCase :: PU XmlUseCase
xpUseCase =
	xpElem "useCase" $
	xpWrap ( uncurry5 XmlUseCase, \ (XmlUseCase i n d s ss) -> (i, n, d, s, ss) ) $
	xp5Tuple (xpElem "id" xpText) 
                 (xpElem "name" xpText) 
                 (xpElem "description" xpText) 
                 (xpElem "setup" xpText) 
                 (xpList xpScenario)

xpAspectualUseCase :: PU XmlAspectualUseCase
xpAspectualUseCase = 
	xpElem "aspect"	$
	xpWrap ( uncurry3 XmlAspectualUseCase, \ (XmlAspectualUseCase i n a) -> (i, n,a) ) $
	xpTriple (xpElem "id" xpText) (xpElem "name" xpText) (xpList xpAdvice)	

xpAdvice :: PU XmlAdvice
xpAdvice = 
	xpElem "advice" $
	xpWrap ( uncurry3 XmlAdvice, \ (XmlAdvice t p s) -> (t, p, s) )	$
	xpTriple (xpElem "type" xpText) (xpElem "pointCut" xpText) (xpList xpStep)

xpScenario :: PU XmlScenario
xpScenario = 
	xpElem "flow" $
	xpWrap ( uncurry5 XmlScenario, \ (XmlScenario i d f t s) -> (i, d, f, t, s) ) $
	xp5Tuple (xpElem "id" xpText )
	         (xpElem "description" xpText ) 
		 (xpElem "fromSteps" xpText) 
		 (xpElem "toSteps" xpText) 
		 (xpList xpStep) 

-- xpAdviceFlow :: PU XmlAdviceFlow
-- xpAdviceFlow = 
--         xpElem "aspectualFlow" $
--         xpWrap (XmlAdviceFlow, \ (XmlAdviceFlow s) -> (s) ) $
--        (xpList xpStep) 

xpStep :: PU XmlStep 
xpStep = 
	xpElem "step" $
	xpWrap ( uncurry4 XmlStep, \ (XmlStep i a  s r) -> (i, a, s, r) ) $
	xp4Tuple (xpElem "stepId" xpText) (xpElem "action" xpText ) (xpElem "condition" xpText) (xpElem "response" xpText)

	

