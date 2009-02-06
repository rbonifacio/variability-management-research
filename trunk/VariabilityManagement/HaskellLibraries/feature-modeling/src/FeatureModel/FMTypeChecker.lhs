\subsection{Type Checker for feature models}

%if False
\begin{code}
--| This modulde defines several functions 
-- for feature model type checking
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

\begin{code}
fmTypeChecker :: FeatureModel -> CheckerResult
fmTypeChecker fm = 
 case errors of
  [] -> Success
  xs -> Fail { errorList = errors }
 where
  errors = cRootFeature ++ cConstraints ++ cDuplications ++ cSatisfiable 
  cRootFeature  = featureTC (fmRoot fm) 
  cConstraints  = constraintsTC fm
  cDuplications = noDuplicationsTC fm 
  cSatisfiable  = if (isSatisfiable fm) then [] else ["Feature model is not satisfiable."]
\end{code}

\subsubsection{Feature type checker}

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

The type checker for the global constraints verifies that:

\begin{itemize}
 \item all references are made to features declared in the feature model; 
 \item an expression should not be inconsistent with the feature models restrictions--- eg.: 
       a constraint should not exclude a mandatory feature; and
 \item expressions should not be inconsistent among them--- for instance, the constraints 
       \texttt{A implies B} is inconsistent with \texttt{A implies not B}.
\end{itemize}

For that reason, we have decided to use a SAT solver for checking if the constraints 
of a feature model are satisfiable. Therefore, first of all it would be interesting 
to convert the feature model to expressions in propositional logic. Actually, this 
mainly consists of:
\begin{itemize}
\item create an expression to the root feature, stating that it must be present in 
all members of the product line; and 
\item transform each feature, in the feature model, to a corresponding propositional 
logic expression.  
\end{itemize}
 
The corresponding propostional expression for a feature \emph{f} depends on  
the group type of the feature. For a \emph{Basic Feature}, we state that \emph{f} implies 
all of its mandatory children; for an \emph{Alternative Feature}, we state that \emph{f}
implies at least one child to be selected; and, finally, for an \emph{Or Feature}, we 
state that \emph{f} implies one, and only one, child to be selected. 
The result is a list of \emph{feature expressions} that {\bf must} be satisfied in order to consider 
a product as a valid instance of the feature model.  

\begin{code}
fmToPropositionalLogic :: FeatureModel -> [FeatureExpression]
fmToPropositionalLogic fm =  (ref froot) : (featureToPropositionalLogic froot)               
 where froot = fmRoot fm

featureToPropositionalLogic :: Feature -> [FeatureExpression]
featureToPropositionalLogic  f = (featureToPL f) 
 where 
  featureToPL f =   
   (
    case groupType f of
     BasicFeature-> [(ref f) |=> (ref x) | x <- children f, fType x == Mandatory] 
     OrFeature -> [(ref f) |=> (foldOr [ref x | x <- children f])]  
     AlternativeFeature -> [(ref f) |=> (foldOr [xor x (delete x (children f)) | x <- children f])]  
   ) ++ (childrenToPL f) 
  childrenToPL f = foldr (++) [] [featureToPL c | c <- children f]
  xor f [] = ref f
  xor f xs = And (ref f) (foldAnd [Not (ref x) | x <- xs])
\end{code}

We also have to translate constraints to propositional logic--- or even better, to 
\emph{Feature Expressions}. 

\begin{code}
constraintToPropositionalLogic :: Constraint -> FeatureExpression 
constraintToPropositionalLogic c = 
 case constraintType c of 
  Implies -> lhs |=> rhs 
  Iff     -> And (lhs |=> rhs) (rhs |=>lhs)
 where 
  lhs = constraintLHSExp c
  rhs = constraintRHSExp c
\end{code}

Then, it was also necessary to convert the feature expressions to the Conjunctive 
Normal Form (CNF), since most of the available Haskell SAT solvers expect 
sentences in CNF\footnote{The SAT solver used in this library is the FUNSAT}.  

\begin{code}
fmToCNFExpression :: FeatureModel -> FeatureExpression 
fmToCNFExpression fm = 
 let
  fmExpressions = fmToPropositionalLogic fm
  cExpressions  = map constraintToPropositionalLogic (fmConstraints fm)
 in toCNFExpression (foldAnd (fmExpressions ++ cExpressions)) 
\end{code}

The \texttt{fmSATSolver} function returns a configuration of 
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

Auxiliarly funcions were defined for converting feature expressions 
to the CNF format and to the Dimacs CNF format.These functions are not
represented in this report.


%if False
\begin{code} 

toCNFExpression :: FeatureExpression -> FeatureExpression
toCNFExpression (And e1 e2)    = And (toCNFExpression e1) (toCNFExpression e2)
toCNFExpression (Or e1 e2)     = distributeAndOverOr e1 e2
toCNFExpression (Not e1)       = moveNotInwards e1
toCNFExpression (FeatureRef f) = (FeatureRef f) 

distributeAndOverOr :: FeatureExpression -> FeatureExpression -> FeatureExpression  
distributeAndOverOr (And x y) e2 = And (toCNFExpression (Or x e2)) (toCNFExpression(Or y e2)) 
distributeAndOverOr e1 (And x y) = And (toCNFExpression(Or e1 x)) (toCNFExpression(Or e1 y))
distributeAndOverOr e1 e2 = distributeAndOverOr' a b 
 where 
  distributeAndOverOr' (And x y) e = toCNFExpression (Or (And x y) e)
  distributeAndOverOr' e (And x y) = toCNFExpression (Or e (And x y))
  distributeAndOverOr' x y = Or (toCNFExpression x) (toCNFExpression  y) 
  a = toCNFExpression e1
  b = toCNFExpression e2
 
moveNotInwards :: FeatureExpression -> FeatureExpression
moveNotInwards (And x y) = Or  (toCNFExpression (Not x)) (toCNFExpression (Not y))
moveNotInwards (Or x y)  = And (toCNFExpression (Not x)) (toCNFExpression (Not y))
moveNotInwards (Not x)   = toCNFExpression x
moveNotInwards e         =  Not e  

dimacsFormat :: FeatureExpression -> CNF 
dimacsFormat exp = 
 let 
  vars = getVars exp
  cs = map (expToLiterals vars) (getClauses exp)
 in CNF {
   numVars = length vars,
   numClauses = length cs,
   clauses =  Set.fromList cs
 } 

getVars :: FeatureExpression -> [FeatureExpression] 
getVars (And exp1 exp2)  = nub ((getVars exp1) ++ (getVars exp2))
getVars (Or  exp1 exp2)  = nub ((getVars exp1) ++ (getVars exp2))
getVars (Not exp1)       =  getVars exp1
getVars (FeatureRef f) = [(FeatureRef f)]
getVars otherwise      = []

getClauses :: FeatureExpression -> [FeatureExpression]
getClauses (And exp1 exp2) = (getClauses exp1) ++ (getClauses exp2)
getClauses (Or  exp1 exp2) = [Or exp1 exp2]
getClauses (Not exp1)      = [Not exp1]
getClauses (FeatureRef f)  = [FeatureRef f]
getClauses otherwhise      = [] 

expToLiterals :: [FeatureExpression] -> FeatureExpression -> Clause
expToLiterals fs e = map intToLiteral (expToLiterals' fs e)
 where
  expToLiterals' fs (Or  exp1 exp2) = (expToLiterals' fs exp1) ++ (expToLiterals' fs exp2) 
  expToLiterals' fs (Not exp1)      = map (*(-1)) (expToLiterals' fs exp1)
  expToLiterals' fs (FeatureRef f)  = 
   if isJust (elemIndex (FeatureRef f) fs)
    then [fromJust(elemIndex (FeatureRef f) fs) + 1] 
    else []
  expToLiterals' fs otherwise = []
  intToLiteral x = L { unLit = x }
\end{code}
%endif




\begin{code}

constraintsTC :: FeatureModel -> ErrorList
constraintsTC fm = foldr (++) [] [constraintTC' c | c <- fmConstraints fm]
 where 
  constraintTC' c = (checkReferences c)
  checkReferences c = checkExpReferences (constraintLHSExp c) ++ checkExpReferences (constraintRHSExp c)  
  checkExpReferences e = if (eval fc e) then [] else [("Expression " ++ (show e) ++ " is not valid")]
  fc = FeatureConfiguration { fcRoot = (fmRoot fm) } -- a fc with all features of the fm
\end{code}

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