
module UseCaseModel2Model (addScenariosM2M, bindParametersM2M)
where

import Maybe
import BasicTypes
import UseCaseModel
import FeatureModel
import List


addScenariosM2M :: [Id] -> UseCaseModel -> UseCaseModel -> UseCaseModel
addScenariosM2M ids input output = 
 let ins = ucmScenarios input
     outUseCases = useCases output
     name = ucmName output
   in UCM name (addUseCaseM2M input output  [s | s <- ins, exists (scenarioId s) ids])


bindParametersM2M :: Name -> Feature -> FeatureConfiguration -> UseCaseModel -> UseCaseModel
bindParametersM2M pName feature  fc  output = 
 let f = findFeatureFromConfiguration fc (fId feature)
     name = ucmName output
 in 
  if isNothing f then error "..."
  else UCM name (bindUseCasesParametersM2M pName (fromJust f) [uc | uc <- useCases output])

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
   then UCM (ucmName ucm) (ucs ++ [UseCase oucId oucName oucDescription [sc]]) 
   else UCM (ucmName ucm) ((delete (fromJust ouc) ucs) ++ [UseCase oucId oucName oucDescription (ucScenarios (fromJust ouc) ++ [sc])])
   
   
   