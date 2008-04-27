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
import BasicTypes
-- import FeatureModel
-- import Environment 

type Action = String
type State = String
type Response = String
type ScenarioList = [Scenario]
type StepList = [Step] 
type FromStep = [StepRef] 
type ToStep = [StepRef]
type Annotation = String

-- A scenario has an Id, a Description, a sequence of steps and references
-- for "from" and "to" steps. A step is defined with an Id, a reference to 
-- the scenario, and the related user action, system state and system response. 
-- A use case is a group of close related scenarios
data UseCaseModel = UCM Name [UseCase]
	 deriving (Show)
	 
data UseCase = UseCase Id Name Description [Scenario]
	 deriving (Show)
	 
data Scenario = Scenario Id Description FromStep StepList ToStep
	 deriving (Show) 
	 
data Step = Step Id Scenario Action State Response [Annotation]
	 deriving (Show)
	 
data StepRef = IdRef Id | AnnotationRef String
	 deriving (Show)
	 
-- *********************************************************
-- Use case model access functions
-- *********************************************************
useCases :: UseCaseModel -> [UseCase]
useCases (UCM name ucs) = ucs

-- **********************************************************
-- Use case access functions
-- **********************************************************
useCase :: UseCase -> Id
useCase (UseCase id _ _ _) = id

scenarios :: UseCase -> [Scenario]
scenarios (UseCase _ _ _ scenarios) = scenarios

-- ********************************************************** 
-- Scenario access functions 
-- **********************************************************
scenarioId :: Scenario -> Id 
scenarioId (Scenario id description from steps to) = id

write :: Scenario -> String
write (Scenario id description from steps to) = description

steps :: Scenario -> StepList
steps (Scenario id description from steps to) = steps

from :: Scenario -> FromStep
from (Scenario id description from steps to) = from

to :: Scenario -> ToStep
to  (Scenario id description from steps to) = to

-- ***********************************************************
-- Step access functions
-- ***********************************************************
stepId :: Step -> Id
stepId (Step id _ _ _ _ _) = id

owner :: Step -> Scenario
owner (Step _ scenario _ _ _ _)  = scenario

annotations :: Step -> [String]
annotations (Step _ _ _ _ _ annotationList) = annotationList

stepListIds :: StepList -> [String]
stepListIds l = [stepId x | x <- l]

details :: Step -> String
details (Step id scenario action state response annotationList) = 
 "Action: " ++ action ++ " State:" ++ state ++ " Response: " ++ response

-- ***********************************************************
-- A Step must be an instance of Eq. In this way, it is 
-- possible to test equality as (step1 == step2). The equality 
-- is based on the step id.
-- ***********************************************************
instance Eq Step where 
 Step id1 _ _ _ _ _ == Step id2 _ _ _ _ _ = id1 == id2

-- 
-- This function retrieve all steps in a use case model that 
-- staisfies the the reference StepRef
--  
matchAll :: UseCaseModel -> [StepRef] -> StepList
matchAll _ [] = []
matchAll ucm (r:rs) = 
 [s | s <- (extractStepsFromScenarios (extractScenariosFromUCs (useCases ucm))), match s r] ++ matchAll ucm rs

-- match
match :: Step -> StepRef -> Bool
match step (IdRef id) = if (stepId step) == id then True else False
match step (AnnotationRef ann) = exists ann (annotations step)  
 
 

-- **********************************************************
-- Compute the complete steps of a given scenario.
-- This take in consideration, in a recursive way, all 
-- from steps and to steps.
--
-- TODO: This function must be changed. We need to retrieve 
-- the list from steps using [match x | x <- (from scenario)] or
-- [match x | x<- (to scenario)]
-- **********************************************************
completePaths :: UseCaseModel -> Scenario -> [StepList]
completePaths ucm scenario = 
 (fromList ucm (matchAll ucm (from scenario)) +++ [steps scenario]) +++ (toList ucm (matchAll ucm (to scenario)))

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
-- Operator that distribute the concat operator over lists.
-- The result is:
-- [[1M, 2M, 3M], [A11, A12]] +++ [[A21, A22, A23]] = 
-- [[1M, 2M, 3M, A21, A22, A23], [A11, A12, A21, A22, A23]] 
-- *************************************************************

(+++) :: [StepList] -> [StepList] -> [StepList]
steps1 +++ steps2 = [ x ++ y | x<-steps1 , y <- steps2]


extractScenariosFromUCs :: [UseCase] -> ScenarioList
extractScenariosFromUCs [] = [] 
extractScenariosFromUCs (x:xs) = (scenarios x) ++ (extractScenariosFromUCs xs)

extractStepsFromScenarios :: ScenarioList -> StepList
extractStepsFromScenarios [] = [] 
extractStepsFromScenarios (x:xs) = (steps x) ++ (extractStepsFromScenarios xs)


allPathsFromScenarioList :: UseCaseModel -> [Scenario] -> [StepList] 
allPathsFromScenarioList ucm [] = []
allPathsFromScenarioList ucm (x:xs) = (completePaths ucm x) ++ (allPathsFromScenarioList ucm xs)

-- **********************************************************
-- Iddle scenario definition
-- **********************************************************
ucIddle = UseCase "0" "IDDLE Use Case" "The iddle use case" [idle] 
idle = Scenario "0" "IDDLE" [] [start, end] []
start = Step "start" idle "START" "" "" []
end   = Step "end" idle "END" "" "" []
