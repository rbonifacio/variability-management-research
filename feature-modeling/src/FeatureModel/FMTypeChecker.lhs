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
fmSATSolver fm = solve1 (dimacsFormat (fmToCNFExpression fm))

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
constraintsTC fm = 
 let 
  cnames = [(c,n) | c <- fmConstraints fm, n <- expNames (constraintToPropositionalLogic c)]
  fnames = [fId f | f <- plainFeature (fmRoot fm)]
  inames = [(c,n) | (c,n) <- cnames, notElem n fnames] --invalid references of the constraint 
 in ["Invalid reference " ++ n  ++ " found in constraint " ++ show (c) | (c,n) <- inames] 
 
 

expNames :: FeatureExpression -> [String] 
expNames (And e1 e2)    = (expNames e1) ++ (expNames e2)
expNames (Or  e1 e2)    = (expNames e1) ++ (expNames e2)
expNames (Not e1)       = expNames e1
expNames (FeatureRef e) = [e]
expNames otherwise    = []  
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
 let 
  fnames = [fId f | f <- plainFeature (fmRoot fm)]
  dupps  = [f | f <- fnames, length (filter (==f) fnames) > 1]
 in ["Feature " ++ f ++ " is duppliecated." | f <- dupps] 
 
\end{code}

\section{Detecting bad smells}

\begin{code}
missingAlternatives :: FeatureModel -> [BadSmell]
missingAlternatives fm = missingAlternative' r 
 where 
  missingAlternative' f =  
   (
    case groupType f of 
    BasicFeature -> []
    otherwise -> if (length (children f) > 1) 
     then [] 
     else ["Expecting more than one child in Feature " ++ show(f) ]
   ) ++ (concat [missingAlternative' x | x <- children f])
  r = fmRoot fm

constraintImposingAlternatives :: FeatureModel -> [BadSmell]
constraintImposingAlternatives fm = 
 let 
  ecs  = filter (expectedConstraint fm) (fmConstraints fm)
 in ["Constraint " ++ (show c) ++ " imposes alternative feature." | c <- ecs, impliesFeatures (altfIds fm) c]
  
constraintRequiringOptional :: FeatureModel -> [BadSmell]
constraintRequiringOptional fm = 
 let 
  ecs = filter (expectedConstraint fm) (fmConstraints fm) 
 in ["Constraint " ++ (show c) ++ " imposes optional feature." | c <- ecs, impliesFeatures (optfIds fm) c]


checkDeadFeatures :: FeatureModel -> [BadSmell]
checkDeadFeatures fm = [ "Dead feature: " ++ show f | f <- checkDeadFeatures' fm]
 where 
  checkDeadFeatures' fm = [f | f <- plainFeature (fmRoot fm), fType f == Optional,  not (isSatisfiable (addImpliesFeature fm f))]

addImpliesFeature :: FeatureModel -> Feature -> FeatureModel
addImpliesFeature fm f = 
 let 
  cs = fmConstraints fm 
  c = Constraint {
        constraintType = Implies ,
        constraintLHSExp = ref (fmRoot fm),
        constraintRHSExp = ref (f)
      }
  in 
   fm { fmConstraints = c : cs}

  
optfIds :: FeatureModel -> [String]
optfIds fm = [fId f | f <- plainFeature (fmRoot fm), (fType f) == Optional]

altfIds :: FeatureModel -> [String]
altfIds fm = 
 map fId (concat [children f | f <- plainFeature (fmRoot fm), groupType f == AlternativeFeature]) 
    

impliesFeatures :: [String] -> Constraint -> Bool
impliesFeatures altf c = 
 let names = expNames (constraintRHSExp c)
 in (intersect names altf) == names
  
expectedConstraint :: FeatureModel -> Constraint -> Bool
expectedConstraint fm c = 
 let 
  c1 = Constraint {
         constraintType = Implies,
         constraintLHSExp = ref (fmRoot fm),
         constraintRHSExp = Not (constraintLHSExp c) 
       }
  fmt = fm { fmConstraints = [c1] }
 in not (isSatisfiable fmt)
 
type BadSmell = String 

findBadSmells :: FeatureModel -> [BadSmell]
findBadSmells fm = 
 if (isSatisfiable fm)
  then 
   (missingAlternatives fm) ++
   (constraintImposingAlternatives fm) ++
   (constraintRequiringOptional fm) ++ 
   (checkDeadFeatures fm)
 else
  ["The feature model is inconsistent (unsatisfiable)"]
 
 
\end{code}