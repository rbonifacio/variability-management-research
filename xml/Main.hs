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
--					] "completo.xml"
--	     >>>
--	     processUseCaseModel
--	     >>>
--	     xpickleDocument xpUseCaseModel [ (a_indent, v_1)
--				      ] "new-completo.xml"
--	   )
--      return ()
-- 
---- the dummy for processing the unpickled data
-- 
--processUseCaseModel	:: IOSArrow XmlUseCaseModel XmlUseCaseModel
--processUseCaseModel
--    = arrIO ( \ x -> do {print (xmlUseCaseModel2UseCaseModel x) ; return x})


main :: IO ()
main = do 
	[x] <- runX ( xunpickleDocument xpUseCaseModel [ (a_validate,v_0)
					, (a_trace, v_1)
					, (a_remove_whitespace,v_1)
					, (a_preserve_comment, v_0)
					] "completo.xml" )
	[y] <-  runX ( xunpickleDocument xpFeature [ (a_validate,v_0)
					, (a_trace, v_1)
					, (a_remove_whitespace,v_1)
					, (a_preserve_comment, v_0)
					] "feature-model.xml" )				
	print x
	print y				