-----------------------------------------------------------------------------
-- |
-- Module      :  RequirementModel.Types
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- It defines the requirement model, a very very simple model
-- that describes the SPL requirements. Basically, an SPL requirement
-- has an id, a name and a description; whereas the requirement model 
-- has a list of requirements. 
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
module RequirementModel.Types
where 

import BasicTypes

import Data.Generics

-- | 
-- The SPL requirement model data type.
-- 
data RequirementModel = RM {
      reqs :: [Requirement]
} deriving (Show, Eq, Data, Typeable)

-- | 
-- Just a SPL requirement, witch has 
-- an id, name and description. 
-- 
data Requirement = Requirement {
      reqId :: Id, 
      reqName :: String,
      reqDescription :: String
} deriving (Show, Data, Typeable)

-- The equality test of a requirement is based on 
-- its id.
instance Eq Requirement where 
 x == y = ((reqId x) == (reqId y))





