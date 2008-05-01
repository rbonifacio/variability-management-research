module Main where

import Text.XML.HXT.Arrow
import System.Environment

import XmlUseCaseParser
import XmlFeatureParser
import XmlUseCaseModel
import XmlFeatureModel

--main	:: IO ()
--main
--    = do
--      runX ( xunpickleDocument xpUseCaseModel [ (a_validate,v_0)
--					, (a_trace, v_1)
--					, (a_remove_whitespace,v_1)
--					, (a_preserve_comment, v_0)
--					] "test.xml"
--	     >>>
--	     processUseCaseModel
--	     >>>
--	     xpickleDocument xpUseCaseModel [ (a_indent, v_1)
--				      ] "test.xml"
--	   )
--      return ()
 
-- the dummy for processing the unpickled data
 
--processUseCaseModel	:: IOSArrow XmlUseCaseModel XmlUseCaseModel
--processUseCaseModel
--    = arrIO ( \ x -> do {print (xmlUseCaseModel2UseCaseModel x) ; return x})

--main	:: IO ()
--main
--    = do
--      runX ( xunpickleDocument xpUseCase [ (a_validate,v_0)
--					, (a_trace, v_1)
--					, (a_remove_whitespace,v_1)
--					, (a_preserve_comment, v_0)
--					] "scenario.xml"
--	     >>>
--	     processUseCase
--	     >>>
--	     xpickleDocument xpUseCase [ (a_indent, v_1)
--				      ] "new-scenario.xml"
--	   )
--      return ()
-- 
---- the dummy for processing the unpickled data
-- 
--processUseCase	:: IOSArrow UseCase UseCase
--processUseCase
--    = arrIO ( \ x -> do {print x ; return x})

--main	:: IO ()
--main
--    = do
--      runX ( xunpickleDocument xpScenario [ (a_validate,v_0)
--					, (a_trace, v_1)
--					, (a_remove_whitespace,v_1)
--					, (a_preserve_comment, v_0)
--					] "steps.xml"
--	     >>>
--	     processScenario
--	     >>>
--	     xpickleDocument xpScenario [ (a_indent, v_1)
--				      ] "new-steps.xml"
--	   )
--      return ()
-- 
---- the dummy for processing the unpickled data
-- 
--processScenario	:: IOSArrow XmlScenario XmlScenario
--processScenario
--    = arrIO ( \ x -> do {print x ; return x})


main	:: IO ()
main
    = do
      runX ( xunpickleDocument xpFeature [ (a_validate,v_0)
					, (a_trace, v_1)
					, (a_remove_whitespace,v_1)
					, (a_preserve_comment, v_0)
					] "feature-model.xml"
	     >>>
	     processFeature
	     >>>
	     xpickleDocument xpFeature [ (a_indent, v_1)
				      ] "new-feature-model.xml"
	   )
      return ()
 
-- the dummy for processing the unpickled data
 
processFeature	:: IOSArrow XmlFeature XmlFeature
processFeature
    = arrIO ( \ x -> do {print (xmlFeature2Feature x) ; return x})