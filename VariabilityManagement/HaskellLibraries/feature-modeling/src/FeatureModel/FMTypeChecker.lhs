\section{Type Checker for feature models}

The type checker for a feature model iterates over the 
features (starting from the root element) and checks if each 
feature is well typed. Additionally, 

\begin{itemize} 
\item The global constraints of the feature model must be well typed;  
\item Features should appear only once in the feature model; while  the equality property 
of a feature considers just the \emph{feature id} property; and
\item The feature model must be satisfiable, which means that at least one product configuration 
must satisfy the feature model relationships and constraints.    
\end{itemize}

%if False
\begin{code}
module FeatureModel.FMTypeChecker where 

import FeatureModel.Types

import Funsat.Types
import Funsat.Solver
import Funsat.Resolution

import List
import qualified Data.Set as Set

import Maybe
\end{code}
%endif

\begin{code}
fmTypeChecker :: FeatureModel -> CheckerResult
fmTypeChecker fm = 
 case errors of
  [] -> Success
  xs -> Fail { errorList = errors }
 where
  errors = if (e1) == [] then cSatisfiable else e1
  e1 = cRootFeature ++ cConstraints ++ cDuplications   
  cRootFeature  = featureTC (fmRoot fm) 
  cConstraints  = constraintsTC fm
  cDuplications = noDuplicationsTC fm 
  cSatisfiable  = if (isSatisfiable fm) then [] else ["Feature model is not satisfiable."]
\end{code}

\subsection{Feature type checker}

The type checker for a feature checks if an \texttt{alternative} feature or 
an {or} feature had defined at least one child.  

\begin{code}
featureTC :: Feature -> ErrorList
featureTC feature = foldr (++) [] [featureTC' x | x <- plainFeature feature] 
 where 
  featureTC' FeatureError = ["FeatureError is not expected in a feature model"]
  featureTC' f = 
   if ((groupType f) /= BasicFeature) && (length (children f) == 0)  
    then [("Expecting at least one child for feature" ++ (fId f))] 
    else []  
\end{code}

\subsection{Global constraints and satisfiability type checkers}

The type checker for the global constraints verifies that:

\begin{itemize}
 \item all references are made to features declared in the feature model; 
 \item an expression should not be inconsistent with the feature models restrictions--- eg.: 
       a constraint should not exclude a mandatory feature; and
 \item expressions should not be inconsistent among them--- for instance, the constraints 
       \texttt{A implies B} is inconsistent with \texttt{A implies not B}.
\end{itemize}

For that reason, we have decided to use a SAT solver for checking if the constraints 
of a feature model are satisfiable. The \texttt{fmSATSolver} function returns a configuration of 
features that satisfies the FM--- if it is satisfiable. Besides that, 
the \texttt{isSatisfiable} function returns True iff a feature model is 
satisfiable. 
 
\begin{code}
fmSATSolver :: FeatureModel -> (Solution, Stats, Maybe ResolutionTrace) 
fmSATSolver fm = solve1 (dimacsFormat (fmToTseitinEncode fm))

isSatisfiable :: FeatureModel -> Bool 
isSatisfiable fm = 
 let 
  (x, y, z) = fmSATSolver fm
 in case x of 
  Sat a  -> True
  otherwise -> False
\end{code}

Additionally, we have to check if the constraints refer to features declared 
in the feature model. 

\begin{code}
constraintsTC :: FeatureModel -> ErrorList
constraintsTC fm = foldr (++) [] [constraintTC' c | c <- fmConstraints fm]
 where 
  constraintTC' c = (checkReferences c)
  checkReferences c = checkExpReferences (constraintLHSExp c) ++ checkExpReferences (constraintRHSExp c)  
  checkExpReferences e = 
   if (eval fc e) then [] else [("Expression " ++ (show e) ++ " is not valid")]
  fc = FeatureConfiguration { fcRoot = (fmRoot fm) } 
\end{code}

\subsection{Duplication type checker}

In order to check for duplicated features, we just need 
to compare if the number of features of the feature model 
\texttt{fm} is equal to the number of features after excluding 
any duplication. If it is not the case, there are duplicated 
features.  

 
\begin{code}
noDuplicationsTC :: FeatureModel -> ErrorList
noDuplicationsTC fm = 
 if (length xs == length (nub xs)) then [] 
 else ["There are duplicated features in the feature model"] 
 where  xs = plainFeature (fmRoot fm)
\end{code}