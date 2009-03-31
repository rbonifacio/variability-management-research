\begin{code}
module UseCaseModel.Transformations -- (selectScenarios, bindParameter, evaluateAdvice)
where

import BasicTypes
import UseCaseModel.Types
import FeatureModel.Types
import ConfigurationKnowledge.Types

import Data.Generics
import Maybe
import List

selectScenarios :: [Id] -> SPLModel -> InstanceModel -> InstanceModel
selectScenarios ids spl product = 
 addScenariosToInstance (scs, spl, product)
 where scs = [s | s <- splScenarios spl, scId s `elem` ids]

bindParameter :: Id -> Id -> SPLModel -> InstanceModel -> InstanceModel
bindParameter pid fid spl product = 
 bindParameter' steps (unwords options) pid product
 where 
  steps = [s | s <- ucmSteps (snd product), s `refers` pid]
  options = concat (map featureOptionsValues [f | f <- flatten (fcTree (fst product)), fId (fnode f) == fid]) 
  bindParameter'[] o pid p = p
  bindParameter' (s:ss) o pid p = bindParameter' ss o pid (gReplaceStringInStep (sId s) pid o p) 

evaluateAspect:: [Id] -> SPLModel -> InstanceModel -> InstanceModel
evaluateAspect ids spl product = 
 evaluateAdvices as product
 where 
  as = concat [advices a | a <- aspects (snd spl), (aspectId a) `elem` ids]

evaluateAdvices :: [Advice] -> InstanceModel -> InstanceModel
evaluateAdvices [] p = p
evaluateAdvices (x:xs) p = evaluateAdvices xs (genEvaluateAdvice x p)

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

genEvaluateAdvice :: Advice -> InstanceModel -> InstanceModel  
genEvaluateAdvice a = everywhere (mkT (evaluateAdvice a))

  

    

refers :: Step -> Id -> Bool
refers s pid = 
 let 
  w = "{" ++ pid ++ "}" 
  wExists = existsWord w
 in (wExists (action s)) || (wExists (state s)) || (wExists (response s))
    
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
 

gReplaceStringInStep :: Id -> String -> String -> InstanceModel -> InstanceModel
gReplaceStringInStep sid old new = everywhere (mkT (replaceStringInStep sid old new)) 


{-- 
=========================================
= Auxiliarly functions to add scenarios
========================================= 
--}
addScenariosToInstance :: ([Scenario], SPLModel, InstanceModel) -> InstanceModel
addScenariosToInstance ([], spl, product) = product
addScenariosToInstance ((s:ss), spl, product) = addScenariosToInstance (ss, spl, product') 
 where product' = addScenarioToInstance (s, spl, product) 

addScenarioToInstance :: (Scenario, SPLModel, InstanceModel) -> InstanceModel
addScenarioToInstance (s, spl, product) = 
 let 
  sUseCase = findUseCaseFromScenario (useCases (snd spl)) s
  pUseCase = findUseCaseFromScenario (useCases (snd product)) s
  eUseCase = (emptyUseCase sUseCase) { ucScenarios = [s] } 
 in case pUseCase of
     Nothing -> gAddUseCase eUseCase product
     Just u  -> gAddScenario (ucId u) s product

addOrReplaceScenario :: Id -> Scenario -> UseCase -> UseCase
addOrReplaceScenario i sc uc =
 if (ucId uc == i) 
  then 
   let scs = ucScenarios uc 
   in uc { ucScenarios = sc : [s | s <- scs, s /= sc] }
  else uc 

addOrReplaceUseCase :: UseCase -> UseCaseModel -> UseCaseModel
addOrReplaceUseCase uc ucm = 
 let ucs = useCases ucm 
 in ucm { useCases = uc : [u | u <- ucs, u /= uc ] }

-- generic function for adding a scenario (scrap your boilerplate (SYB) pattern )
gAddScenario :: Id -> Scenario -> InstanceModel -> InstanceModel
gAddScenario i s = everywhere (mkT (addOrReplaceScenario i s))

-- generic function for adding a use case (SYB pattern) 
gAddUseCase :: UseCase -> InstanceModel -> InstanceModel 
gAddUseCase u = everywhere (mkT (addOrReplaceUseCase u))

emptyUseCase :: Maybe UseCase -> UseCase
emptyUseCase Nothing = error "Can not create a use case from Nothing" 
emptyUseCase (Just uc) = uc { ucScenarios = [] }

splScenarios spl = ucmScenarios (snd spl)
   
{-- 
=========================================
= Auxiliarly functions to bind parameters
========================================= 
--}






-- addScenariosM2M :: [Id] -> SPL -> ProductInstance -> ProductInstance
-- addScenariosM2M ids spl productInstance = 
--  let 
--  	inputUCM = splUseCaseModel spl
--  	outputUCM = instanceUseCaseModel productInstance
-- 	ins = ucmScenarios inputUCM
-- 	outUseCases = useCases outputUCM
-- 	rName = ucmName outputUCM
-- 	rUseCases = addUseCaseM2M inputUCM outputUCM  [s | s <- ins, exists (scenarioId s) ids]
-- 	rAspects = aspects outputUCM
--    in 
--     productInstance { 
--    	 instanceUseCaseModel = UCM rName rUseCases rAspects
--    	}

-- bindParametersM2M :: Name -> Id -> SPL -> ProductInstance -> ProductInstance
-- bindParametersM2M pName feature  spl productInstance = 
--  let 
--    fc = instanceConfiguration productInstance
--    outputUCM = instanceUseCaseModel productInstance
--    ft = findFeatureFromConfiguration fc feature   
--  in 
--    if isNothing ft then error "..."
--    else 
--    	let
--    	 rName = ucmName outputUCM 
--    	 rUseCases = bindUseCasesParametersM2M pName (fromJust ft) [uc | uc <- useCases outputUCM]
--    	 rAspects = aspects outputUCM 
--    	in 
--      productInstance { 
--    	  instanceUseCaseModel = UCM  rName rUseCases rAspects     	  	  
--    	 }

-- -- TODO: devemos passar o nome do aspecto.
-- -- dessa forma, eh necessario recuperar o aspecto 
-- -- no modelo de caso de uso da linha de produto 
-- evaluateAspectM2M :: Name -> SPL -> ProductInstance -> ProductInstance
-- evaluateAspectM2M name spl productInstance = 
--  let
--    aspect = head [a | a <- (aspects (splUseCaseModel spl)), (aspectId a) == name] 
--    outputUCM = instanceUseCaseModel productInstance	
--    adviceList = advices aspect
--    rName = ucmName outputUCM
--    rUseCases = evaluateAdvices outputUCM [advice | advice <- adviceList]
--    rAspects = aspects outputUCM
--  in 
--   productInstance {
--   	instanceUseCaseModel = UCM rName rUseCases rAspects
--   }
 
-- evaluateAdvices :: UseCaseModel -> [Advice] -> [UseCase] 
-- evaluateAdvices ucm [] = useCases ucm 
-- evaluateAdvices ucm (x:xs) = 
--   let 
--    pc = pointCut x
--    matchedSteps = matchAll ucm pc
--    ucs = useCases ucm
--   in evaluateAdvices (ucm {useCases = (evaluateAdvice ucs matchedSteps x)}) xs
 
-- evaluateAdvice :: [UseCase] -> StepList -> Advice -> [UseCase]
-- evaluateAdvice ucs [] advice = ucs
-- evaluateAdvice ucs (x:xs) advice = 
--   let 
--    baseScenario = owner x 
--    useCase = getUseCaseFromScenario ucs baseScenario 
--    outScenario = composeScenarioWithAdvice x baseScenario advice
--    outUseCase = replaceScenarioInUseCase (fromJust useCase) outScenario
--   in evaluateAdvice (replaceElement (fromJust useCase) outUseCase ucs) xs advice

-- --     
-- composeScenarioWithAdvice :: Step -> Scenario -> Advice -> Scenario
-- composeScenarioWithAdvice step base advice = 
--  let 
--   composition = if (isBeforeAdvice advice) then (composeBefore step) else (composeAfter step)
--   outSteps = composition (steps base) (steps (aspectualScenario advice))  
--  in base {steps = outSteps}

-- --  
-- bindUseCasesParametersM2M :: Name -> Feature -> [UseCase] -> [UseCase]
-- bindUseCasesParametersM2M pName f useCases = 
--  [UseCase (ucId uc) 
--           (ucName uc) 
--           (ucDescription uc) 
--           (bindScenariosParametersM2M pName f(ucScenarios uc)) | uc <- useCases
--  ]
 
-- bindScenariosParametersM2M  :: Name -> Feature -> [Scenario] -> [Scenario]
-- bindScenariosParametersM2M pName f scenarios = 
--  [Scenario (scenarioId s)
--            (scenarioDescription s) 
--            (from s)
--            (bindStepsParametersM2M pName f (steps s))
--            (to s) | s <- scenarios
--   ]
 
-- bindStepsParametersM2M :: Name -> Feature -> [Step] -> [Step]
-- bindStepsParametersM2M pName f [] = []
-- bindStepsParametersM2M pName f (x:xs) = 
--  let step = if (exists pName (extractParametersFromStep x)) 
--              then bindStepParameterM2M pName f x
--              else x
--   in [step] ++ bindStepsParametersM2M pName f (xs)     
 
-- bindStepParameterM2M  :: Name -> Feature -> Step -> Step
-- bindStepParameterM2M pName f step = 
--  Step (stepId step) 
--       (owner step) 
--       (replaceParameter pName f (action step) ) 
--       (replaceParameter pName f (state step) )
--       (replaceParameter pName f (response step) )
--       (annotations step)

-- extractParametersFromStep :: Step -> [String]
-- extractParametersFromStep step = 
--  let str = unwords [(action step), (state step), (response step)] 
--   in findDelimitedString str '<' '>'  
  
-- replaceParameter :: Name -> Feature -> String -> String
-- replaceParameter pName f s = 
--  let values = concatValueList (optionValues  (featureOptions (f)))
--   in replace s pName (values)     
      

-- --  

-- addUseCaseM2M :: UseCaseModel -> UseCaseModel -> [Scenario] -> [UseCase]
-- addUseCaseM2M input output [] = useCases output
-- addUseCaseM2M input output (x:xs) = 
--  let inputUC = getUseCaseFromScenario (useCases input) x
--   in if (isNothing inputUC)
--    then error "addUseCaseM2M: Scenario not present in the use case model"
--    else addUseCaseM2M input (addOrUpdateUseCaseM2M (fromJust inputUC) output x) xs   
   
-- addOrUpdateUseCaseM2M :: UseCase -> UseCaseModel ->  Scenario -> UseCaseModel      
-- addOrUpdateUseCaseM2M uc ucm sc = 
--  let ucs = useCases ucm
--      ouc = firstElement ucs uc
--      oucId = ucId uc 
--      oucName = ucName uc 
--      oucDescription = ucDescription uc
     
--   in if (isNothing ouc) 
--    then UCM (ucmName ucm) (ucs ++ [UseCase oucId oucName oucDescription [sc]]) (aspects ucm) 
--    else UCM (ucmName ucm) ((delete (fromJust ouc) ucs) ++ [UseCase oucId oucName oucDescription (ucScenarios (fromJust ouc) ++ [sc])]) (aspects ucm)

\end{code}
   
   
   