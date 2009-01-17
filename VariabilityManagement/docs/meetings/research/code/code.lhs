\documentclass{article}

%include lhs2tex.fmt
%include lhs2tex.sty

%format not = "{\bf{not}}"
%format <- = "\in"

\begin{document}
\large
\begin{code}
type ConfigurationKnowledge = [Configuration]
 
data Configuration  = Configuration {
 expression :: FeatureExpression,
 transformations :: [Model2Model] 	 
}

build spl fc ck = stepRefinement [(x spl) | x <- ts] sm
 where 
  ts = [transformations c| c <- ck, eval fc (expression c)]
  sm = (emptyInstance spl fc)   	
 
\end{code}

\newpage

\begin{code}
exp1 = FeatureRef "FEA-01" -- eShop feature
exp2 = FeatureRef "FEA-14" -- shopping cart
exp3 = FeatureRef "FEA-15" -- register user preferences
exp4 = not exp2 
exp5 = not exp3
...

conf1 = (exp1, [(addScenariosM2M ["1"])])
conf2 = (exp2, [(evaluateAspectM2M aucShoppingCart)])
conf3 = (exp3, [(evaluateAspectM2M aucBasicFlow)])
conf4 = (exp4, [(evaluateAspectM2M aucUpdatePreferences)])
conf5 = (exp5, [(bindParametersM2M "ShipMethod" (fId shipMethod))])
...

configuration = [conf1,conf2, conf3, conf4, conf5,...]

type Model2Model = (SPL -> ProductInstance) -> ProductInstance

addScenariosM2M :: [Id] -> Model2Model
addScenariosM2M ids spl productInstance = ...

bindParametersM2M :: Name -> Feature -> Model2Model
bindParametersM2M pName feature  spl productInstance = ...

evaluateAspectM2M :: AspectualUseCase -> Model2Model
evaluateAspectM2M aspect spl productInstance = ...

\end{code}

\newpage

\begin{code}
data UseCaseModel = UCM {
	 	ucmName :: Name,  
	 	useCases :: [UseCase],
	 	aspects :: [AspectualUseCase]
	 }
	 
data UseCase = UseCase {
		ucId :: Id,  
		ucName :: Name,
		ucDescription :: Description ,
		ucScenarios :: ScenarioList 
	}
	 
data Scenario = Scenario {
		scenarioId :: Id,
		scenarioDescription :: Description,
		steps :: StepList
	}

data Step = Step {
		stepId :: Id,  
		action :: Action,
		state ::  State,
		response :: Response, 
		annotations :: [Annotation]
	}
	
data AspectualUseCase = AspectualUseCase {
		aspectId :: Id,
		aspectName :: Name,
		advices :: [Advice]
	} 
	

data Advice = 
 BeforeAdvice {pointCut :: [StepRef], aspectualScenario :: Scenario}  | 
 AfterAdvice { pointCut :: [StepRef], aspectualScenario :: Scenario }

\end{code}

\end{document}