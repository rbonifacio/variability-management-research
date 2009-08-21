-----------------------------------------------------------------------------
-- |
-- Module      :  Transformations.RequirementModel
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Several transformations that instantiate a product's use case model 
-- from a SPL use case model 
--
-----------------------------------------------------------------------------
module Trasformations.RequirementModel
where 

import RequirementModel.Types
import ConfigurationKnowledge.Types

-- | 
-- Transformation that selects all requirements 
-- of a product line. It is usefull when most of 
-- the requirements are present in all instances 
-- of the SPL member.
selectAllRequirements :: SPLModel      -> -- ^ the SPL model 
                         InstanceModel -> -- ^ the current version of the instance model
                         InstanceMoDEL    -- ^ the new version of the instance model

selectAllRequirements spl product = product { req = rs } 
 where rs = splReq spl

-- |
-- Transformation that select some requirements of 
-- a product line. 
-- 
selectRequirements :: [Id]          -> -- ^ the ids of SPL requirements that should be selected
                      SPLModel      -> -- ^ the SPL model
                      InstanceModel -> -- ^ the current version of the instance model
                      InstanceModel    -- ^ the new version of the instance model

selectRequirements ids spl product = product { req = rs } 
 where 
  selected = [r | r <- (splReq spl) , (reqId r) `elem` ids]
  rs = nub $ (req prodcuts) ++ selected

removeRequirements :: [Id]     ->      -- ^ the ids of SPL requirements that should be removed from the product
                      SPLModel ->      -- ^ the SPL model. It does not hava many utilities here.
                      InstanceModel -> -- ^ the current version of the instance model
                      InstanceModel    -- ^ the new version of the instance model

removeRequirements ids spl product = product { req = rs } 
 where 
  rs = [r | <- (req product), not ((reqId r) `elem` ids)]

                      