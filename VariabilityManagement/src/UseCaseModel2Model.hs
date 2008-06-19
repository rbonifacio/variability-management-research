
module UseCaseModel2Model (addScenariosM2M)
where

import Maybe
import BasicTypes
import UseCaseModel

addScenariosM2M :: [Id] -> UseCaseModel -> UseCaseModel -> UseCaseModel
addScenariosM2M ids input output = 
 let ins = ucmScenarios input
     outUseCases = useCases output
     name = ucmName output
     addedUseCases = [addUseCaseM2M input output s | s <- ins, exists (scenarioId s) ids]
  in UCM name (disjointConcatenation addedUseCases outUseCases)


addUseCaseM2M :: UseCaseModel -> UseCaseModel -> Scenario -> UseCase
addUseCaseM2M input output sc = 
 let inputUC = getUseCaseFromScenario (useCases input) sc 
  in if (isNothing inputUC)
      then error "addUseCaseM2M: Scenario not present in the use case moel"
      else addOrUpdateUseCaseM2M (fromJust inputUC) (useCases output) sc
      
addOrUpdateUseCaseM2M :: UseCase -> [UseCase] -> Scenario -> UseCase      
addOrUpdateUseCaseM2M uc ucs sc = 
 let ouc = firstElement ucs uc
     oucId = ucId uc 
     oucName = ucName uc 
     oucDescription = ucDescription uc
  in if (isNothing ouc) 
   then UseCase oucId oucName oucDescription [sc]
   else UseCase oucId oucName oucDescription (ucScenarios (fromJust ouc) ++ [sc])