-----------------------------------------------------------------------------
-- |
-- Module      :  UseCaseModel.Transformations
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
--
-----------------------------------------------------------------------------
module Transformations.SourceCode
where 

import BasicTypes
import ConfigurationKnowledge.Types

-- | Transformation that adds some mappings to the 
--   TextMappings property f an instance model. This is 
--   an example of a very trivial transformation. 

selectComponents :: [Key]            -- ^ component identifier
                -> SPLModel       -- ^ SPL with the input use case model. Not very useful here.
                -> InstanceModel  -- ^ current version of the instance model. it declares the current mappings.
                -> InstanceModel  -- ^ refined version of the product specific use case model.
selectComponents keys spl product = 
 let 
  scs = [snd x | x <- splMappings spl, fst x `elem` keys] 
  ics = components product
 in case scs of 
     []  -> product -- mapping not found. we have to report this somehow
     otherwise -> product { components = ics ++ scs }
     -- should the case (x:xs) be considered a problem?


