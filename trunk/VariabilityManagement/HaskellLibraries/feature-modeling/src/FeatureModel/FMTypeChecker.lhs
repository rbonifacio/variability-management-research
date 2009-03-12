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
  e1 = (checkFeatureTree (fmTree fm)) ++ (checkConstraints fm) ++ (checkDuplications fm)   
  cSatisfiable  = if (isSatisfiable fm) then [] else ["Feature model is not satisfiable."]
\end{code}

\subsection{Feature type checker}

The type checker for a feature checks if an \texttt{alternative} feature or 
an {or} feature had defined at least one child.  

\begin{code}
checkFeatureTree :: FeatureTree -> [ErrorMessage]
checkFeatureTree ftree = foldFTree (++) (checkFeature') (checkFeature') [] ftree 
 where
  checkFeature' ftree = 
   if ((groupType (fnode ftree)) /= BasicFeature) && (length (children ftree) == 0)  
    then [("Expecting at least one child for feature" ++ (fId (fnode ftree)))] 
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
checkConstraints :: FeatureModel -> [ErrorMessage]
checkConstraints fm = 
 let 
  cnames = [(c,n) | c <- fmConstraints fm, n <- expNames c]
  fnames = [fId (fnode f) | f <- flatten (fmTree fm)]
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
checkDuplications :: FeatureModel -> [ErrorMessage]
checkDuplications fm = 
 let 
  fnames = [fId (fnode f) | f <- flatten (fmTree fm)]
  dupps  = [f | f <- fnames, length (filter (==f) fnames) > 1]
 in ["Feature " ++ f ++ " is duppliecated." | f <- dupps] 
 
\end{code}

\section{Detecting bad smells}


\begin{code}
missingAlternatives :: FeatureModel -> [Feature]
missingAlternatives fm = [ fnode f 
                         | f <- flatten (fmTree fm)
                         , groupType (fnode f) /= BasicFeature 
                         , length (children f) < 2
                         ]

checkDeadFeatures :: FeatureModel -> [Feature]
checkDeadFeatures fm = [ fnode f 
                       | f <- flatten (fmTree fm) 
                       , fType (fnode f) == Optional
                       , not (isSatisfiable (addConstraint fm (ref (fnode f))))
                       ]

type BadSmell = String 

findBadSmells :: FeatureModel -> [BadSmell]
findBadSmells fm = 
 if (isSatisfiable fm)
  then 
   ["Expecting at least 2 children in feature " ++ show (f) | f <- missingAlternatives fm] ++
   ["Feature " ++ show f ++ " is a dead feature" | f <- checkDeadFeatures fm]
 else
  ["The feature model is inconsistent (unsatisfiable)"]


addConstraint :: FeatureModel -> FeatureExpression -> FeatureModel
addConstraint fm exp = fm { fmConstraints = exp : cs}
 where cs = fmConstraints fm 



-- constraintImpliesAlternative :: FeatureModel -> [Constraint]
-- constraintImpliesAlternative fm = [ c 
--                                   | c <- fmConstraints fm
--                                   , expectedImpliesConstraint c
--                                   , c `impliesFeaturesIn` (alternativeFeatures fm)
--                                   ]  
  
-- findAlternativeIds :: FeatureModel -> [Feature]
-- findAlternativeIds fm = [ fId (fnode c) 
--                         | c <- children f
--                         , f <- flatten (fmTree fm)   
--                         , fType (fNode f) = AlternativeFeature 
--                         ]

-- impliesFeaturesIn :: Constraint -> [String] -> Bool
-- impliesFeaturesIn (Or (Not e1) (FeatureRef r)) ids = r `elem` ids
-- impliesFeaturesIn _ fs = False 

  
-- constraintRequiringOptional :: FeatureModel -> [BadSmell]
-- constraintRequiringOptional fm = 
--  let 
--   ecs = filter (expectedConstraint fm) (fmConstraints fm) 
--  in ["Constraint " ++ (show c) ++ " imposes optional feature." | c <- ecs, impliesFeatures (optfIds fm) c]


-- optfIds :: FeatureModel -> [String]
-- optfIds fm = [fId f | f <- plainFeature (fmRoot fm), (fType f) == Optional]

-- altfIds :: FeatureModel -> [String]
-- altfIds fm = 
--  map fId (concat [children f | f <- plainFeature (fmRoot fm), groupType f == AlternativeFeature]) 
    

-- impliesFeatures :: [String] -> Constraint -> Bool
-- impliesFeatures altf c = 
--  let names = expNames (constraintRHSExp c)
--  in (intersect names altf) == names
  
-- expectedImpliesConstraint :: FeatureModel -> Constraint -> Bool
-- expectedImpliesConstraint fm (Or (Not e1) e2) = 
--  let 
--   checkedConstraint = (Not e1)
--   newFeatureModel = fm { fmConstraints = [checkedConstraint] }
--  in not (isSatisfiable fmt)
 
 
 
\end{code}