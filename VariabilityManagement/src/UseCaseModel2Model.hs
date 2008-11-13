
module UseCaseModel2Model (addScenariosM2M, bindParametersM2M, evaluateAspectM2M)
where

import Maybe
import BasicTypes
import UseCaseModel
import FeatureModel
import ProductLineModel
import List


addScenariosM2M :: [Id] -> SPL -> ProductInstance -> ProductInstance
addScenariosM2M ids spl productInstance = 
 let 
 	inputUCM = splUseCaseModel spl
 	outputUCM = instanceUseCaseModel productInstance
	ins = ucmScenarios inputUCM
	outUseCases = useCases outputUCM
	rName = ucmName outputUCM
	rUseCases = addUseCaseM2M inputUCM outputUCM  [s | s <- ins, exists (scenarioId s) ids]
	rAspects = aspects outputUCM
   in 
    productInstance { 
   	 instanceUseCaseModel = UCM rName rUseCases rAspects
   	}

bindParametersM2M :: Name -> Id -> SPL -> ProductInstance -> ProductInstance
bindParametersM2M pName feature  spl productInstance = 
 let 
   fc = instanceConfiguration productInstance
   outputUCM = instanceUseCaseModel productInstance
   ft = findFeatureFromConfiguration fc feature   
 in 
   if isNothing ft then error "..."
   else 
   	let
   	 rName = ucmName outputUCM 
   	 rUseCases = bindUseCasesParametersM2M pName (fromJust ft) [uc | uc <- useCases outputUCM]
   	 rAspects = aspects outputUCM 
   	in 
     productInstance { 
   	  instanceUseCaseModel = UCM  rName rUseCases rAspects     	  	  
   	 }

-- TODO: devemos passar o nome do aspecto.
-- dessa forma, eh necessario recuperar o aspecto 
-- no modelo de caso de uso da linha de produto 
evaluateAspectM2M :: Name -> SPL -> ProductInstance -> ProductInstance
evaluateAspectM2M name spl productInstance = 
 let
   aspect = head [a | a <- (aspects (splUseCaseModel spl)), (aspectId a) == name] 
   outputUCM = instanceUseCaseModel productInstance	
   adviceList = advices aspect
   rName = ucmName outputUCM
   rUseCases = evaluateAdvices outputUCM [advice | advice <- adviceList]
   rAspects = aspects outputUCM
 in 
  productInstance {
  	instanceUseCaseModel = UCM rName rUseCases rAspects
  }
 
evaluateAdvices :: UseCaseModel -> [Advice] -> [UseCase] 
evaluateAdvices ucm [] = useCases ucm 
evaluateAdvices ucm (x:xs) = 
  let 
   pc = pointCut x
   matchedSteps = matchAll ucm pc
   ucs = useCases ucm
  in evaluateAdvices (ucm {useCases = (evaluateAdvice ucs matchedSteps x)}) xs
 
evaluateAdvice :: [UseCase] -> StepList -> Advice -> [UseCase]
evaluateAdvice ucs [] advice = ucs
evaluateAdvice ucs (x:xs) advice = 
  let 
   baseScenario = owner x 
   useCase = getUseCaseFromScenario ucs baseScenario 
   outScenario = composeScenarioWithAdvice x baseScenario advice
   outUseCase = replaceScenarioInUseCase (fromJust useCase) outScenario
  in evaluateAdvice (replaceElement (fromJust useCase) outUseCase ucs) xs advice

--     
composeScenarioWithAdvice :: Step -> Scenario -> Advice -> Scenario
composeScenarioWithAdvice step base advice = 
 let 
  composition = if (isBeforeAdvice advice) then (composeBefore step) else (composeAfter step)
  outSteps = composition (steps base) (steps (aspectualScenario advice))  
 in base {steps = outSteps}

--  
bindUseCasesParametersM2M :: Name -> Feature -> [UseCase] -> [UseCase]
bindUseCasesParametersM2M pName f useCases = 
 [UseCase (ucId uc) 
          (ucName uc) 
          (ucDescription uc) 
          (bindScenariosParametersM2M pName f(ucScenarios uc)) | uc <- useCases
 ]
 
bindScenariosParametersM2M  :: Name -> Feature -> [Scenario] -> [Scenario]
bindScenariosParametersM2M pName f scenarios = 
 [Scenario (scenarioId s)
           (scenarioDescription s) 
           (from s)
           (bindStepsParametersM2M pName f (steps s))
           (to s) | s <- scenarios
  ]
 
bindStepsParametersM2M :: Name -> Feature -> [Step] -> [Step]
bindStepsParametersM2M pName f [] = []
bindStepsParametersM2M pName f (x:xs) = 
 let step = if (exists pName (extractParametersFromStep x)) 
             then bindStepParameterM2M pName f x
             else x
  in [step] ++ bindStepsParametersM2M pName f (xs)     
 
bindStepParameterM2M  :: Name -> Feature -> Step -> Step
bindStepParameterM2M pName f step = 
 Step (stepId step) 
      (owner step) 
      (replaceParameter pName f (action step) ) 
      (replaceParameter pName f (state step) )
      (replaceParameter pName f (response step) )
      (annotations step)

extractParametersFromStep :: Step -> [String]
extractParametersFromStep step = 
 let str = unwords [(action step), (state step), (response step)] 
  in findDelimitedString str '<' '>'  
  
replaceParameter :: Name -> Feature -> String -> String
replaceParameter pName f s = 
 let values = concatValueList (optionValues  (featureOptions (f)))
  in replace s pName (values)     
      

--  

addUseCaseM2M :: UseCaseModel -> UseCaseModel -> [Scenario] -> [UseCase]
addUseCaseM2M input output [] = useCases output
addUseCaseM2M input output (x:xs) = 
 let inputUC = getUseCaseFromScenario (useCases input) x
  in if (isNothing inputUC)
   then error "addUseCaseM2M: Scenario not present in the use case model"
   else addUseCaseM2M input (addOrUpdateUseCaseM2M (fromJust inputUC) output x) xs   
   
addOrUpdateUseCaseM2M :: UseCase -> UseCaseModel ->  Scenario -> UseCaseModel      
addOrUpdateUseCaseM2M uc ucm sc = 
 let ucs = useCases ucm
     ouc = firstElement ucs uc
     oucId = ucId uc 
     oucName = ucName uc 
     oucDescription = ucDescription uc
     
  in if (isNothing ouc) 
   then UCM (ucmName ucm) (ucs ++ [UseCase oucId oucName oucDescription [sc]]) (aspects ucm) 
   else UCM (ucmName ucm) ((delete (fromJust ouc) ucs) ++ [UseCase oucId oucName oucDescription (ucScenarios (fromJust ouc) ++ [sc])]) (aspects ucm)
   
   
   