module XmlUseCaseModel where

import List 

import UseCaseModel
import BasicTypes

type XmlAction = String
type XmlState = String
type XmlResponse = String
type XmlScenarioList = [XmlScenario]
type XmlStepList = [XmlStep]
type XmlFromStep = String
type XmlToStep = String

-- A scenario has an Id, a Description, a sequence of steps and references
-- for "from" and "to" steps. A step is defined with an Id, a reference to 
-- the scenario, and the related user action, system state and system response. 
-- A use case is a group of close related scenarios
data XmlUseCaseModel = XmlUCM Name [XmlUseCase] [XmlAspectualUseCase]
	 deriving (Show)

data XmlUseCase = XmlUseCase Id Name Description XmlScenarioList 
     deriving (Show)
     
data XmlAspectualUseCase = XmlAspectualUseCase { 
 	xmlAspectName :: Name,
 	xmlAdvices :: [XmlAdvice] 
 }     
 deriving (Show)
     
data XmlScenario = XmlScenario Description XmlFromStep XmlToStep XmlStepList
	 deriving (Show)

data XmlAdvice = XmlAdvice {
	xmlAdviceType :: String,
	xmlPointcut :: String,
	xmlAdviceScenario :: XmlScenario	
 }
 deriving (Show) 
	 
data XmlStep = XmlStep Id XmlAction XmlState XmlResponse 	 
	 deriving (Show)
	 
xmlUseCaseModel2UseCaseModel :: XmlUseCaseModel -> UseCaseModel
xmlUseCaseModel2UseCaseModel (XmlUCM name xmlUseCases xmlAspects) = 
	UCM name 
	    [xmlUseCase2UseCase xmlUseCase | xmlUseCase <- xmlUseCases] 
	    [xmlAspectualUseCase2AspectualUseCase xmlAspect | xmlAspect <- xmlAspects]

-- TODO: Note that all scenarios have the same id
xmlUseCase2UseCase :: XmlUseCase -> UseCase
xmlUseCase2UseCase (XmlUseCase i n d xmlScenarios) = 
 UseCase  i n  d [(xmlScenario2Scenario ("SC-") xmlScenario) | xmlScenario <- xmlScenarios] 

xmlAspectualUseCase2AspectualUseCase :: XmlAspectualUseCase -> AspectualUseCase
xmlAspectualUseCase2AspectualUseCase xmlAspect = 
 AspectualUseCase {
 	aspectName = (xmlAspectName xmlAspect),
 	advices = [xmlAdvice2Advice xmlAdvice | xmlAdvice <- (xmlAdvices xmlAspect)]
 }

xmlAdvice2Advice :: XmlAdvice -> Advice
xmlAdvice2Advice xmlAdvice = 
 let 
 	sc = xmlScenario2Scenario "" (xmlAdviceScenario xmlAdvice) -- TODO: what should be the id of an aspectual scenario?
 	refs = xmlStepRefs2StepRefs (xmlPointcut xmlAdvice)
 	fn = case (xmlAdviceType xmlAdvice) of 
 			"before" -> BeforeAdvice 
 			"after" -> AfterAdvice
 	in fn refs sc		
  
-- 
-- the string parameter was required because the UseCase xml document 
-- has no element "id" for scenarios.
-- 
xmlScenario2Scenario :: String -> XmlScenario -> Scenario
xmlScenario2Scenario s (XmlScenario description fromSteps toSteps steps) = 
 scenario where 
 scenario = Scenario s 
 			 		description 
 			 		(xmlStepRefs2StepRefs fromSteps) 
 			 		[xmlStep2Step scenario step | step <- steps] 
 			 		(xmlStepRefs2StepRefs toSteps)
 
xmlStep2Step :: Scenario -> XmlStep -> Step
xmlStep2Step scenario (XmlStep i a s r) = 
 Step i scenario a s r []
  
-- This function is used to retreve a list of step refs 
-- from a string. It was implemented since the use case xml 
-- document group different from steps and to steps in a single 
-- string. So, there is no fromStep element for each reference.  
--
-- TODO: There is no support to annotations
xmlStepRefs2StepRefs :: String -> [StepRef]
xmlStepRefs2StepRefs s = [IdRef x | x <- (splitAndRemoveBlanks ',' s)]        
