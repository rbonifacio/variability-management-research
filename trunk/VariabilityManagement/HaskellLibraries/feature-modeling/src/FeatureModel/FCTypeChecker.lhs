\subsection{Type checker for feature configurations}

%if False
\begin{code}
module FeatureModel.FCTypeChecker where 
\end{code}
%endif 

The \texttt{validInstance} is the top most function for 
checking if a feature configuration (fc) is a valid instance 
of a feature model (fm). In order to do that, it is necessary to 
check if the root feature of those models are equal. Then, 
each constraint of the feature model must be considered by 
the configuration. 
 
\begin{code}
validInstance :: FeatureModel -> FeatureConfiguration -> ErrorList
validInstance fm fc = 
 let f1 = fmRoot fm
     f2 = fcRoot fc
 in 
  if (f1 == f2) then checkFeatures f1 f2
  else ["The root elements must be the same"]
\end{code}

\subsubsection{Type checker for a selected feature}

The type checker for a selected feature (the second parameter)
verifies that it complies to the feature constraints 
defined in the feature model. Such a verification relies 
on the group type of the feature.

\begin{code}
checkFeatures :: Feature -> Feature -> ErrorList
checkFeatures FeatureError fc = [("Feature " ++ (fId fc) ++ " not expected")]
checkFeatures fm  FeatureError = [("Expecting feature " ++ (fId fm))]
checkFeatures fm fc = 
 case (groupType fm) of 
   BasicFeature -> checkBasicFeature fm fc
   AlternativeFeature -> checkAlternativeFeature fm fc  
   OrFeature -> checkOrFeature fm fc
\end{code}

checkBasicFeature :: Feature -> Feature -> ErrorList
checkBasicFeature fm fc = 
 (checkMandatoryFeatures fm fc) ++ (checkOptionalFeatures fm fc) 


checkMandatoryFeatures :: Feature -> Feature -> ErrorList
checkMandatoryFeatures fm fc = 
 foldr (++) [] [checkFeatures x (findChildFeature x fc) | x <- children fm, fType x == Mandatory] 
   
checkOptionalFeatures :: Feature -> Feature -> ErrorList
checkOptionalFeatures fm fc = 
 (foldr (++) [] [checkFeatures x y | x <- children fm, y <- children fc, x == y, fType x == Optional]) ++
 (foldr (++) [] [checkFeatures FeatureError y | y <- children fc, (findChildFeature y fm) == FeatureError])

checkAlternativeFeature :: Feature -> Feature -> ErrorList
checkAlternativeFeature fm fc = 
 case children fc of 
  []     -> [("Exactly one child must be selected for feature " ++ (fId fm))]
  [x]    ->  checkFeatures (findChildFeature x fm) x 
  (x:xs) -> [("Exactly one child must be selected for feture " ++ (fId fm))] 

checkOrFeature :: Feature -> Feature -> ErrorList
checkOrFeature fm fc = 
 case children fc of 
  [] -> [("At least one child must be selected for feature " ++ (fId fm))]
  xs -> foldr (++) [] [checkFeatures (findChildFeature x fm) x | x <- xs] 

\end{code}

The \texttt{eval} function checks if a feature configuration satisfies 
the constraints represented in the feature expression. 

\begin{code}
eval :: FeatureConfiguration -> FeatureExpression -> Bool
eval config (FeatureRef ref) = featureExists ref (fcRoot config)
eval config (Not e) = not (eval config e)
eval config (And e1 e2) = (eval config e1) && (eval config e2)
eval config (Or e1 e2) = (eval config e1) || (eval config e2)
eval _ (ConstantExpression e) = e
\end{code}

\begin{code}
existError :: ErrorList -> Bool
existError [] = False
existError (x:xs) = True
\end{code}
