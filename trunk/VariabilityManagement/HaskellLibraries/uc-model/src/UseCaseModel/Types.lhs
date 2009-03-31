\begin{code}
{-
   
   RequirementVariabilityManament.hs


   This source code define the main concepts (types like 
   use case, scenario, steps, and so on) related to
   the requirements variability context. Also, this code 
   implement some functions that represent the weaving 
   process of main flow and alternativ flow variability 
   mechanisms.

   Author: Rodrigo Bonifacio 
-}

{-# OPTIONS -fglasgow-exts #-}

module UseCaseModel.Types where

-- import Prelude hiding ( (^) )
import List 

import Maybe

import Data.Generics

import BasicTypes

type Description = String
type Action = String
type State = String
type Response = String
type FromStep = [StepRef] 
type ToStep = [StepRef]
type Annotation = String

-- A scenario has an Id, a Description, a sequence of steps and references
-- for "from" and "to" steps. A step is defined with an Id, a reference to 
-- the scenario, and the related user action, system state and system response. 
-- A use case is a group of close related scenarios
data UseCaseModel = UCM {
 ucmName :: Name,  
 useCases :: [UseCase],
 aspects :: [AspectualUseCase]
} deriving (Show, Typeable, Data)

data UseCase = UseCase {
 ucId :: Id,  
 ucName :: Name,
 ucDescription :: Description,
 ucScenarios :: [Scenario] 
} deriving (Show, Typeable, Data)
	 
data Scenario = Scenario {
 scId :: Id,
 scDescription :: Description,
 from :: FromStep,
 steps :: [Step], 
 to :: ToStep
} deriving (Show, Typeable, Data) 

data StepRef = IdRef Id | AnnotationRef String 
 deriving (Show, Typeable, Data)
	 
data Step = Step {
 sId :: Id,  
 owner :: Scenario,
 action :: Action,
 state ::  State,
 response :: Response, 
 annotations :: [Annotation]
} deriving (Typeable, Data)

instance Eq Step where 
 s1 == s2 = sId s1 == sId s2
	 
data AspectualUseCase = AspectualUseCase {
 aspectId :: Id,
 aspectName :: Name,
 advices :: [Advice]
} deriving (Show, Typeable, Data)

data Advice = 
 BeforeAdvice { pointCut :: [StepRef], aspectualScenario :: Scenario }  | 
 AfterAdvice  { pointCut :: [StepRef], aspectualScenario :: Scenario }
 deriving(Show, Typeable, Data) 

stepListIds :: [Step] -> [String]
stepListIds xs = map sId xs 

-- 
-- This function retrieves all steps in a use case model that 
-- statisfies the references StepRef
--  
matchAll :: UseCaseModel -> [StepRef] -> [Step]
matchAll _ [] = []
matchAll ucm (r:rs) = [s | s <- ss, match s r] ++ matchAll ucm rs
 where  ss = concat [steps s | s <- (ucmScenarios ucm)] 
  
-- 
-- This function checks if a given step matches with a 
-- stepref. Notice that a StepRef migh be either an step id or 
-- an annotation. In the first case, the step will match with 
-- the step ref iff both have the same id. In the second 
-- case, a step will match with an annotation ref iff the 
-- step has the specific annotation. 
--
match :: Step -> StepRef -> Bool
match step (IdRef idref) = (sId step) == idref 
match step (AnnotationRef x) = elem x (annotations step)  

-- *************************************************************
-- This function return all scenarios from a use case model.
-- *************************************************************
ucmScenarios :: UseCaseModel -> [Scenario]
ucmScenarios ucm = concat [ucScenarios uc | uc <- useCases ucm] 

ucmSteps :: UseCaseModel -> [Step]
ucmSteps ucm = concat [steps s | s <- ucmScenarios ucm]

findUseCase :: Id -> UseCase -> Maybe UseCase
findUseCase i uc@(UseCase i' _ _ _)
            | i == i'   = Just uc
            | otherwise = Nothing   

findScenario :: Id -> Scenario -> Maybe Scenario
findScenario i sc@(Scenario i' _ _ _ _)
             | i == i'   = Just sc
             | otherwise = Nothing

findStep :: Id -> Step -> Maybe Step 
findStep i s@(Step i' _ _ _ _ _)
         | i == i'   = Just s
         | otherwise = Nothing

  
findScenarioFromStep :: [Scenario] -> Step -> Maybe Scenario
findScenarioFromStep [] st = Nothing
findScenarioFromStep (x:xs) st = 
 if (length [s | s <- steps x, s == st] > 0) 
  then Just x
  else findScenarioFromStep xs st
 
findUseCaseFromScenario :: [UseCase] -> Scenario -> Maybe UseCase
findUseCaseFromScenario [] sc = Nothing 
findUseCaseFromScenario (x:xs) sc  = 
 if (length [s | s <- ucScenarios x, s == sc] > 0)
  then Just x
  else findUseCaseFromScenario xs sc
  
-- replaceStepInScenario :: Step -> Scenario -> Scenario
-- replaceStepInScenario replaced sc =
--  let ss = steps sc
--  in if replaced `elem` ss 
--      then sc { steps = replaced : [s | s <- ss, s /= replaced] }
--      else sc

-- replaceScenarioInUseCase :: UseCase -> Scenario -> UseCase
-- replaceScenarioInUseCase useCase replaced = 
--  let sc = ucScenarios useCase 
--  in  useCase { ucScenarios = (replaced : [s | s <- sc, s /= replaced]) }   
 
-- replaceUseCaseInUCM :: UseCaseModel -> UseCase -> UseCaseModel
-- replaceUseCaseInUCM ucm replaced = 
--  let ucs = useCases ucm
--  in  ucm { useCases = (replaced : [u | u <- ucs, ucId u /= ucId replaced])} 

instance Eq Scenario where 
  s1 == s2 = scId s1 == scId s2
  
instance Eq UseCase where 
  uc1 == uc2 = ucId uc1 == ucId uc2 
 
instance Show Step where
 show (Step i _ action state response _)  = i  ++ " " ++ action ++ " " ++ state ++ response 

 
-- -- 
-- -- Compute the complete steps of a given scenario.
-- -- This take in consideration, in a recursive way, all 
-- -- from steps and to steps.
-- -- 
-- completePaths :: UseCaseModel -> Scenario -> Sequences
-- completePaths ucm scenario = 
--  let fromSteps = matchAll ucm (from scenario)
--      toSteps = matchAll ucm (to scenario)
--   in (fromList ucm (fromSteps) +++ [steps scenario]) +++ (toList ucm (toSteps))

-- fromList :: UseCaseModel -> StepList -> [StepList]
-- fromList ucm [] = []
-- fromList ucm (x:xs) = 
--   if x == start
--    then ([firstElements (steps (owner x)) (x)]) ++ (fromList ucm xs) 
--    else ((fromList ucm (matchAll ucm (from (owner x))) +++ 
--         [firstElements (steps (owner x)) (x)]) ++ (fromList ucm xs))

-- toList :: UseCaseModel -> StepList -> [StepList]
-- toList ucm [] = []
-- toList ucm (x:xs) = 
--  if x == end
--   then ([lastElements (steps (owner x)) (x)]) ++ (toList ucm xs)
--   else ([lastElements (steps (owner x)) (x)] +++ toList ucm (matchAll ucm (to (owner x)))) ++ (toList ucm xs)

-- -- Given a StepList and a Step, return all steps previews to Step 
-- firstElements :: StepList -> Step -> StepList
-- firstElements [] _ = []
-- firstElements (x:xs) y = if x == y then [x] else (x : (firstElements xs y))
-- -- TODO: firstElements (x:xs) y = if x == y then [] else (x : (firstElements xs y))

-- -- Given a StepList and a Step, return all steps defined after Step
-- lastElements :: StepList -> Step -> StepList
-- lastElements x y = reverse (firstElements (reverse x) y)

-- -- *************************************************************  
-- -- Operator that distributee a String over a list of the strings.
-- -- The result is:
-- -- "1M" ^ [[], ["2M"], ["2M", "3M]] = [["1M"], ["1M", "2M"], ["1M", "2M", "3M"]
-- -- *************************************************************

-- (^) :: String -> [[String]] -> [[String]]
-- x ^ y = [ x:e | e<-y ]


-- allPathsFromUCM :: UseCaseModel -> Sequences
-- allPathsFromUCM ucm = 
--  let scenarios = (ucmScenarios ucm) 
--  	in plainList [completePaths ucm x | x <- scenarios] 


-- checkLoopInUCM :: UseCaseModel -> Bool 
-- checkLoopInUCM ucm = False

-- checkLoopInScenario :: UseCaseModel -> Scenario -> ScenarioList 
-- checkLoopInScenario ucm scenario = [] 
-- -- let f sc = matchAll ucm (from sc)
-- --     t sc = matchAll ucm (to sc) 
-- -- in  
-- -- (exists scenario [owner (y) | x <- (f scenario), y <- f (owner x)]) || 
-- -- (exists scenario [owner (y) | x <- (t scenario), y <- t (owner x)])  

-- checkLoopInScenarioClauses :: UseCaseModel -> Scenario -> StepList -> ((Scenario) -> [StepRef]) ->ScenarioList
-- checkLoopInScenarioClauses ucm scenario [] fn = []
-- checkLoopInScenarioClauses ucm scenario (x:xs) fn  =
--  let other = owner x 
--      f fsteps = checkLoopInScenarioClauses ucm scenario fsteps fn 
--  in    
--   if(scenario == other) 
--   then 
--    [other] ++ (f xs) ++ (f (matchAll ucm (fn other)))
--   else
--     (f xs) ++ (f (matchAll ucm (fn other)))
    
-- -- **********************************************************
-- -- Iddle scenario definition
-- -- **********************************************************
-- ucIddle = UseCase "0" "IDDLE Use Case" "The iddle use case" [idle] 
-- idle = Scenario "0" "IDDLE" [] [start, end] []
-- start = Step "start" idle "START" "" "" []
-- end   = Step "end" idle "END" "" "" []

\end{code}
 
