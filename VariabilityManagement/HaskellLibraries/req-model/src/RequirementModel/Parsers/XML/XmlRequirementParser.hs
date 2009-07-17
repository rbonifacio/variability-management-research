-----------------------------------------------------------------------------
-- |
-- Module      :  RequirementModel.Parsers.XmlRequirementParser
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- A XML parser for our requirement model.
--
-----------------------------------------------------------------------------
module RequirementModel.Parsers.XML.XmlRequirementParser
where 

import BasicTypes

import RequirementModel.Types

import Text.XML.HXT.Arrow
import System.Environment

-- | 
-- Parser for a requirement model file. 
-- It results either Success or Fail, if the file is 
-- not valid.
--
parseRequirementModel fileName = 
 do
   c <- runX ( xunpickleDocument xpRequirementModel [ (a_validate,v_0)
 				                    , (a_trace, v_1)
 				                    , (a_remove_whitespace,v_1)
 				                    , (a_preserve_comment, v_0)
                                                    ] fileName )
   case c of 
     [x] -> return $ Success x
     otherwise -> return $ Fail "Error parsing the requirement model. Try to check the input file."

-- 
-- The parser implementation using HXT library.
-- It requires several picklers.
-- 
instance XmlPickler RequirementModel where
	xpickle = xpRequirementModel

instance XmlPickler Requirement where 
	xpickle = xpRequirement

xpRequirementModel :: PU RequirementModel
xpRequirementModel = 
	xpElem "requirementModel" $
	xpWrap ( RM, \ (RM r) -> (r) ) $
        (xpList xpRequirement)		 
			 
xpRequirement :: PU Requirement
xpRequirement = 	
	xpElem   "requirement" $
	xpWrap   ( uncurry3 Requirement, \ (Requirement i n d) -> (i, n, d) ) $
	xpTriple ( xpElem "id" xpText ) 
                 ( xpElem "name" xpText )
                 ( xpElem "description" xpText) 

