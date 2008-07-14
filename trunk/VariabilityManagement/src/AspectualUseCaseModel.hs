
module AspectualUseCaseModel where

import BasicTypes
import UseCaseModel

data AspectualUseCase = AspectualUseCase {
	aspectName :: Name,
	advices :: [Advice]
} 

data Advice = 
 BeforeAdvice {pointCut :: [StepRef], aspectualScenario :: Scenario}  | 
 AfterAdvice { pointCut :: [StepRef], aspectualScenario :: Scenario }
  

isBeforeAdvice :: Advice -> Bool
isBeforeAdvice (BeforeAdvice _ _) = True
isBeforeAdvice (AfterAdvice _ _) = False  
 