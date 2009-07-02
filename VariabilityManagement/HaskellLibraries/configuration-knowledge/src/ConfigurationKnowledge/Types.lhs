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
--
-----------------------------------------------------------------------------
{-# OPTIONS -fglasgow-exts #-}
module ConfigurationKnowledge.Types
where

import Data.Generics

import UseCaseModel.Types

import FeatureModel.Types (FeatureModel, FeatureConfiguration, FeatureExpression)

type Key = String
type Component = String

-- | A type that represents text mappings, which can be used to prepare 
--   a make file. Text mappings usually contain references to 
--   source directories, source files, or configurations files. 
--   Some of the transformations related to source code basically 
--   introduces this kind of mapping. 
type TextFragments = [String] 
type Mapping = (Key, Component)

-- | A type that characterizes an initial representation  
--   of a SPL. Later, we should refactor this type, 
--   introducing new models, such as design, code, and tests.
data SPLModel = SPLModel {
      splFM :: FeatureModel , 
      splUCM :: UseCaseModel, 
      splMappings :: [Mapping] 
} 
          
-- | A type for instances of an SPL. Note that, in this 
--   version, an instance model basically have a feature 
--   configuration and a use case model. Later, other models
--   might be introduced.
data InstanceModel = InstanceModel { 
      fc :: FeatureConfiguration, 
      iucm :: UseCaseModel, 
      components :: [String] 
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
    

 