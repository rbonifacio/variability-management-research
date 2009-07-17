\begin{code}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.SPL.ConfigurationKnowledge
-- Copyright   :  (c) Rodrigo Bonifacio 2008, 2009
-- License     :  LGPL
--
-- Maintainer  :  rba2@cin.ufpe.br
-- Stability   :  provisional
-- Portability :  portable
--
-- Configuration Knowledge in Haskell.
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
module ConfigurationKnowledge.Types
where

import Data.Generics

import BasicTypes
import ComponentModel.Types
import UseCaseModel.Types
import RequirementModel.Types
import FeatureModel.Types (FeatureModel, FeatureConfiguration, FeatureExpression)

-- | A type that characterizes an initial representation  
--   of a SPL. Later, we should refactor this type, 
--   introducing new models, such as design, code, and tests.
data SPLModel = SPLModel {
      splFM  :: FeatureModel,
      splReq :: RequirementModel,  
      splUCM :: UseCaseModel, 
      splMappings :: ComponentModel 
} 
          
-- | A type for instances of an SPL. Note that, in this 
--   version, an instance model basically have a feature 
--   configuration and a use case model. Later, other models
--   might be introduced.
data InstanceModel = InstanceModel { 
      fc  :: FeatureConfiguration,
      req :: RequirementModel,   
      ucm :: UseCaseModel, 
      components :: [Id] 
} deriving (Data, Typeable)
                  

-- | The transformation data type defines a family of 
--   functions that, given an SPL and an Instance Model, 
--   apply some kind of transfomation to the instance model 
--   and then returns a refined version of it. 
type Transformation = SPLModel  -> InstanceModel -> InstanceModel 

-- | A single configuration item. The idea is that, after 
--   evaluating each of the applicable configuration items, 
--   we generate a valid instance of the product line. 
data Configuration = Configuration {
 expression :: FeatureExpression,           -- ^ if expression holds True for a product configuration...
 transformations :: [Transformation] 	    -- ^ the list of transformations would be applied.
} 

-- | The model used to relate feature expressions 
--   to transformations. The configuration knowledge 
--   guides the 'building' process of SPL instances.
type ConfigurationKnowledge = [Configuration]

\end{code}
    

 