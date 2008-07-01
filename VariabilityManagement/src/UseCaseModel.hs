{-
   
   RequirementVariabilityManament.hs


   This source code define the main concepts (types like 
   use case, scenario, steps, and so on) related with 
   the requirement variability context. Also, this code 
   implement some functions that represent the weaving 
   process of main flow and alternativ flow variability 
   mechanisms.

   Author: Rodrigo Bonifacio 
-}

module UseCaseModel where

import Prelude hiding ( (^) )
import List 
import Maybe
import BasicTypes
import AbstractModel

-- import FeatureModel
-- import Environment 

type Action = String
type State = String
type Response = String
type ScenarioList = [Scenario]
type StepList = [Step] 
type Sequences = [StepList]
type FromStep = [StepRef] 
type ToStep = [StepRef]
type Annotation = String

-- A scenario has an Id, a Description, a sequence of steps and references
-- for "from" and "to" steps. A step is defined with an Id, a reference to 
-- the scenario, and the related user action, system state and system response. 
-- A use case is a group of close related scenarios
data UseCaseModel = UCM {
	 	ucmName :: Name,  
	 	useCases :: [UseCase]
	 }
	 deriving (Show)

	 
data UseCase = UseCase {
		ucId :: Id,  
		ucName :: Name,
		ucDescription :: Description ,
		ucScenarios :: ScenarioList 
	}
	 deriving (Show)
	 
data Scenario = Scenario {
		scenarioId :: Id,
		scenarioDescription :: Description,
		from :: FromStep,
		steps :: StepList,
		to :: ToStep
	}
	 deriving (Show) 
	 
data Step = Step {
		stepId :: Id,  
		owner :: Scenario,
		action :: Action,
		state ::  State,
		response :: Response, 
		annotations :: [Annotation]
	}
	 
data StepRef = IdRef Id | AnnotationRef String
	 deriving (Show)
	 
stepListIds :: StepList -> [String]
stepListIds l = [stepId x | x <- l]

details :: Step -> String
details (Step id scenario action state response annotationList) = 
 "Action: " ++ action ++ " State:" ++ state ++ " Response: " ++ response

-- 
-- A Step must be an instance of Eq. In this way, it is 
-- possible to test equality as (step1 == step2). The equality 
-- is based on the step id.
-- 
instance Eq Step where 
 Step id1 _ _ _ _ _ == Step id2 _ _ _ _ _ = id1 == id2

-- 
-- This function retrieves all steps in a use case model that 
-- statisfies the references StepRef
--  
matchAll :: UseCaseModel -> [StepRef] -> StepList
matchAll _ [] = []
matchAll ucm (r:rs) = 
 let steps = extractStepsFromScenarios (ucmScenarios ucm) 
  in [s | s <- steps, match s r] ++ matchAll ucm rs

-- 
-- This function checks if a given step matches with a 
-- stepref. Notice that a StepRef migh be either an step id or 
-- an annotation. In the first case, the step will match with 
-- the step ref iff both have the same id. In the second 
-- case, a step will match with an annotation ref iff the 
-- step has the specific annotation. 
--
match :: Step -> StepRef -> Bool
match step (IdRef id) = 
 if (stepId step) == id 
  then True 
  else False
match step (AnnotationRef x) = exists x (annotations step)  
 
-- 
-- Compute the complete steps of a given scenario.
-- This take in consideration, in a recursive way, all 
-- from steps and to steps.
-- 
completePaths :: UseCaseModel -> Scenario -> Sequences
completePaths ucm scenario = 
 let fromSteps = matchAll ucm (from scenario)
     toSteps = matchAll ucm (to scenario)
  in (fromList ucm (fromSteps) +++ [steps scenario]) +++ (toList ucm (toSteps))

fromList :: UseCaseModel -> StepList -> [StepList]
fromList ucm [] = []
fromList ucm (x:xs) = 
  if x == start
   then ([firstElements (steps (owner x)) (x)]) ++ (fromList ucm xs) 
   else ((fromList ucm (matchAll ucm (from (owner x))) +++ [firstElements (steps (owner x)) (x)]) ++ (fromList ucm xs))

toList :: UseCaseModel -> StepList -> [StepList]
toList ucm [] = []
toList ucm (x:xs) = 
 if x == end
  then ([lastElements (steps (owner x)) (x)]) ++ (toList ucm xs)
  else ([lastElements (steps (owner x)) (x)] +++ toList ucm (matchAll ucm (to (owner x)))) ++ (toList ucm xs)

-- Given a StepList and a Step, return all steps previews to Step 
firstElements :: StepList -> Step -> StepList
firstElements [] _ = []
firstElements (x:xs) y = if x == y then [x] else (x : (firstElements xs y))
-- TODO: firstElements (x:xs) y = if x == y then [] else (x : (firstElements xs y))

-- Given a StepList and a Step, return all steps defined after Step
lastElements :: StepList -> Step -> StepList
lastElements x y = reverse (firstElements (reverse x) y)

-- *************************************************************  
-- Operator that distributee a String over a list of the strings.
-- The result is:
-- "1M" ^ [[], ["2M"], ["2M", "3M]] = [["1M"], ["1M", "2M"], ["1M", "2M", "3M"]
-- *************************************************************

(^) :: String -> [[String]] -> [[String]]
x ^ y = [ x:e | e<-y ]



-- *************************************************************
-- This function return all scenarios from a use case model.
-- *************************************************************
ucmScenarios :: UseCaseModel -> ScenarioList
ucmScenarios ucm = plainList [ucScenarios uc | uc <- useCases ucm] 


-- *************************************************************
-- This function return all steps from a list of 
-- scenarios.
-- *************************************************************
extractStepsFromScenarios :: ScenarioList -> StepList
extractStepsFromScenarios scenarios = plainList [steps s | s <- scenarios]

allPathsFromUCM :: UseCaseModel -> Sequences
allPathsFromUCM ucm = 
 let scenarios = (ucmScenarios ucm) 
 	in plainList [completePaths ucm x | x <- scenarios] 


checkLoopInUCM :: UseCaseModel -> Bool 
checkLoopInUCM ucm = False

checkLoopInScenario :: UseCaseModel -> Scenario -> ScenarioList 
checkLoopInScenario ucm scenario = [] 
-- let f sc = matchAll ucm (from sc)
--     t sc = matchAll ucm (to sc) 
-- in  
-- (exists scenario [owner (y) | x <- (f scenario), y <- f (owner x)]) || 
-- (exists scenario [owner (y) | x <- (t scenario), y <- t (owner x)])  

checkLoopInScenarioClauses :: UseCaseModel -> Scenario -> StepList -> ((Scenario) -> [StepRef]) ->ScenarioList
checkLoopInScenarioClauses ucm scenario [] fn = []
checkLoopInScenarioClauses ucm scenario (x:xs) fn  =
 let other = owner x 
     f fsteps = checkLoopInScenarioClauses ucm scenario fsteps fn 
 in    
  if(scenario == other) 
  then 
   [other] ++ (f xs) ++ (f (matchAll ucm (fn other)))
  else
    (f xs) ++ (f (matchAll ucm (fn other)))
    
-- **********************************************************
-- Iddle scenario definition
-- **********************************************************
ucIddle = UseCase "0" "IDDLE Use Case" "The iddle use case" [idle] 
idle = Scenario "0" "IDDLE" [] [start, end] []
start = Step "start" idle "START" "" "" []
end   = Step "end" idle "END" "" "" []

getUseCaseFromScenario :: [UseCase]-> Scenario -> Maybe UseCase
getUseCaseFromScenario [] sc = Nothing 
getUseCaseFromScenario (x:xs) sc = 
 if (length [s | s <- ucScenarios x, s == sc] > 0)
  then Just x
  else getUseCaseFromScenario xs sc
  
replaceScenarioInUseCase :: UseCase -> Scenario -> UseCase
replaceScenarioInUseCase useCase scenario = 
 let scenarios = ucScenarios useCase 
 in  useCase {ucScenarios = (replaceElement scenario scenario scenarios) }   
 
replaceUseCaseInUCM :: UseCaseModel -> UseCase -> UseCaseModel
replaceUseCaseInUCM ucm useCase = 
 let ucs = useCases ucm
 in  ucm {useCases = (replaceElement useCase useCase ucs)} 
 

instance Eq Scenario where 
  s1 == s2 = scenarioId s1 == scenarioId s2
  
instance Eq UseCase where 
  uc1 == uc2 = ucId uc1 == ucId uc2 
 
instance Show Step where
 show (Step i _ action state response _)  = i  ++ " " ++ action ++ " " ++ state ++ response 
 
instance AbstractModel UseCaseModel where
 emptyModel (ucm) = 
  let name = ucmName ucm in UCM name [] 
 
