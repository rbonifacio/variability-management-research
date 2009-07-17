-----------------------------------------------------------------------------
-- |
-- Module      :  Transformations.UseCaseModel
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
module Transformations.UseCaseModel (selectScenarios, bindParameter, evaluateAspects)
where

import BasicTypes
import UseCaseModel.Types
import FeatureModel.Types
import ConfigurationKnowledge.Types

import Data.Generics
import Maybe
import List


  
-- | Transformation that selects SPL scenarios 
--   from a list of IDs. This transformation deals 
--   with the kind of transformation named 'variability 
--   in control flow'.

selectScenarios :: [Id]           -- ^ scenarios' ids 
                -> SPLModel       -- ^ SPL with the input use case model
                -> InstanceModel  -- ^ current version of the product specific use case model
                -> InstanceModel  -- ^ refined version of the product specific use case model
selectScenarios ids spl product = 
 addScenariosToInstance (scs, spl, product)
 where scs = [s | s <- splScenarios spl, scId s `elem` ids]

-- | Transformation that binds scenario parameters to a selection of 
--   features. It deals with the kind of transformation named 
--   'variability in data'. 

bindParameter :: Id                -- ^ formal parameter of scenarios
              -> Id                -- ^ feature identifier
              -> SPLModel          -- ^ SPL with the input use case model
              -> InstanceModel     -- ^ current version of the product specific use case model
              -> InstanceModel     -- ^ refined version of the product specific use case model
bindParameter pid fid spl product = 
 bindParameter' steps (unwords options) pid product
 where 
  steps = [s | s <- ucmSteps (ucm product), s `refers` pid]
  options = concat (map featureOptionsValues [f | f <- flatten (fcTree (fc product)), fId (fnode f) == fid]) 
  bindParameter'[] o pid p = p
  bindParameter' (s:ss) o pid p = bindParameter' ss o pid (gReplaceStringInStep (sId s) pid o p) 

-- | Transformation that evaluate a list of aspects. 
--   This transformation deals with the kind of variability 
--   named 'variability in control flow'.

evaluateAspects:: [Id]               -- ^ Ids of the aspects that will be evaluated  
                 -> SPLModel        -- ^ SPL with the input use case model
                 -> InstanceModel   -- ^ current version of the product specific use case model
                 -> InstanceModel   -- ^ refined version of the pdocutc specific use case model
evaluateAspects ids spl product = 
 evaluateAdvices as product
 where 
  as = concat [advices a | a <- aspects (splUCM spl), (aspectId a) `elem` ids]

-- evaluate a list of advices
evaluateAdvices :: [Advice] -> InstanceModel -> InstanceModel
evaluateAdvices [] p = p
evaluateAdvices (x:xs) p = evaluateAdvices xs (genEvaluateAdvice x p)

-- evaluate a single advice. This function 
-- implements the weaving process in the lower 
-- granularity level: a flow. 
evaluateAdvice :: Advice -> Flow -> Flow
evaluateAdvice a sf = 
 evaluateAdvice' rs af sf 
 where 
  rs = pointCut a
  af = aspectualFlow a
  fn = case a of 
        BeforeAdvice _ _-> concatBefore
        AfterAdvice _ _-> concatAfter
  evaluateAdvice' [] af sf = sf
  evaluateAdvice' (x:xs) af sf = evaluateAdvice' xs af (fn (match x) af sf) 

-- this is the generic function for evaluating 
-- an advice. It follows the Scrap Your Boilerplate (SYB)
-- pattern. 
genEvaluateAdvice :: Advice -> InstanceModel -> InstanceModel  
genEvaluateAdvice a = everywhere (mkT (evaluateAdvice a))

-- just an auxiliarly function for checking if a 
-- step refers to a parameter. this function might 
-- be parameterized with different delimiters.   
refers :: Step -> Id -> Bool
refers s pid = 
 let 
  w = "{" ++ pid ++ "}" 
  wExists = existsWord w
 in (wExists (action s)) || (wExists (state s)) || (wExists (response s))
    
-- just an auxiliarly function for replacin a string in a step. 
-- actually, it replaces a string at any place of a step: action, 
-- condition, or response. 
replaceStringInStep :: Id -> String -> String -> Step -> Step
replaceStringInStep sid old new step = 
 if (sId step /= sid) 
  then step
  else step { action = rfn a, state = rfn s, response = rfn r }  
   where 
    a = action step
    s = state step
    r = response step  
    rfn = replaceString ("{"++old++"}") new
 
-- this is the generic function for replacing a string into a step (identified 
-- by 'sid'). It follows the SYB pattern. 
gReplaceStringInStep :: Id -> String -> String -> InstanceModel -> InstanceModel
gReplaceStringInStep sid old new = everywhere (mkT (replaceStringInStep sid old new)) 


-- this is a map function that adds a list of scenarios 
-- to a use case model.
addScenariosToInstance :: ([Scenario], SPLModel, InstanceModel) -> InstanceModel
addScenariosToInstance ([], spl, product) = product
addScenariosToInstance ((s:ss), spl, product) = addScenariosToInstance (ss, spl, product') 
 where product' = addScenarioToInstance (s, spl, product) 

-- add a single scenario to a use case model.
addScenarioToInstance :: (Scenario, SPLModel, InstanceModel) -> InstanceModel
addScenarioToInstance (s, spl, product) = 
 let 
  sUseCase = findUseCaseFromScenario (useCases (splUCM spl)) s
  pUseCase = findUseCaseFromScenario (useCases (ucm product)) s
  eUseCase = (emptyUseCase sUseCase) { ucScenarios = [s] } 
 in case pUseCase of
     Nothing -> gAddUseCase eUseCase product
     Just u  -> gAddScenario (ucId u) s product

-- add or replace a scenarion to a use case. this is 
-- an auxiliarly function to the 'selectScenario' transformation.
addOrReplaceScenario :: Id -> Scenario -> UseCase -> UseCase
addOrReplaceScenario i sc uc =
 if (ucId uc == i) 
  then 
   let scs = ucScenarios uc 
   in uc { ucScenarios = sc : [s | s <- scs, s /= sc] }
  else uc 

-- add or replace a use case to a use case model. this is 
-- an auxiliarly function to the 'selectScenario' transformation.
addOrReplaceUseCase :: UseCase -> UseCaseModel -> UseCaseModel
addOrReplaceUseCase uc ucModel = 
 let ucs = useCases ucModel 
 in ucModel { useCases = uc : [u | u <- ucs, u /= uc ] }

-- this is the generic function for adding a scenario.
-- it follows the SYB pattern. 
gAddScenario :: Id -> Scenario -> InstanceModel -> InstanceModel
gAddScenario i s = everywhere (mkT (addOrReplaceScenario i s))

-- this is the generic function for adding a use case. 
-- it follows the SYB pattern. 
gAddUseCase :: UseCase -> InstanceModel -> InstanceModel 
gAddUseCase u = everywhere (mkT (addOrReplaceUseCase u))

emptyUseCase :: Maybe UseCase -> UseCase
emptyUseCase Nothing = error "Can not create a use case from Nothing" 
emptyUseCase (Just uc) = uc { ucScenarios = [] }

splScenarios spl = ucmScenarios (splUCM spl)
   

   
   
   