
module UseCaseModel2Model (addScenariosM2M)
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


--resolveParametersM2M :: Id -> Feature -> UseCaseModel -> UseCaseModel -> UseCaseModel
--resolveParametersM2M parameter feature input output = 
 

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
   
   
   