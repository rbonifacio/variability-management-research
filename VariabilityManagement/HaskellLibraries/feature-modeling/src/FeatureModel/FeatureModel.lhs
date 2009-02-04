
%
% This is the Literate Haskell that defines the main module of the 
% FeatureModeling library.
% 
% author: Rodrigo Bonifacio - rbonifacio@computer.org
%
% 2008/2009
%


\section{Module \texttt{FeatureModel}}

This module defines the main concepts (data types such as 
feature model, features, constraints, and so on) related 
to the feature modeling context. Moreover, this module  
makes available functions for checking if a product configuration 
is a valid instance of a feature model.  
	
%if False
\begin{code}
module FeatureModel.FeatureModel where 

import Funsat.Types
import Funsat.Solver
import Funsat.Resolution

import List
import qualified Data.Set as Set

import Maybe
\end{code}
%endif 

\subsection{Data types}

First of all, we have to declare some data types and synonimous 
for feature modeling, which represent the main 
concepts related to the feature modeling context. Remember that, in Haskell, 
the \texttt{type} construct is used for definining new names for 
existing types.On the other hand, new data types are defined using 
the \texttt{data} construct. New data types might have different functions
for building types. For instance, the data type \texttt{CheckerResult} has 
two constructors: \texttt{Success} and \texttt{Fail}.   

\begin{code}
type Id		   = String
type Name	   = String
type Root          = Feature
type Children      = FeatureList
type Property      = (String, String) 
type Constraints   = [Constraint]
type Properties    = [Property]
type FeatureList   = [Feature]
type ValueList	   = [String]
type ErrorList 	   = [String]
data CheckerResult = Success | Fail { errorList :: ErrorList } deriving (Show)
\end{code}

\texttt{FeatureType} is a new data type for defining if a feature is required or 
not in a feature configuration. Therefore, according to its type, 
a feature can be either an \emph{optional} or a \emph{mandatoty} feature.

\begin{code}
data FeatureType  = Optional | Mandatory 
 deriving (Show, Eq)
\end{code}

Features are also classified according to the semantics of the 
valid configurations of their children. For instance, \texttt{basic} features might 
not have children. On the other hand, \texttt{alternative} and \texttt{or} features 
{\bf must have} children. The difference is that, when selected,  
an \texttt{alternative} feature require exactly one child to be selected. Differently,  
an \texttt{or} feature, when present in a product configuration, 
requires that {\bf at least one of its child} must  be selected. 

\begin{code}
data GroupType = BasicFeature | AlternativeFeature | OrFeature 
 deriving (Show, Eq)
\end{code}

Now we can define the feature model data type, which is basically the root of a rose tree--- whose nodes are 
features--- pluss a list of global constraints. Similarly, a feature configuration, 
or selection of features, is also a tree of features--- although the feature configuration 
data type has just a reference to a root feature. 

\begin{code}
data FeatureModel = FeatureModel {
	fmRoot :: Root,
        fmConstraints :: Constraints 
} deriving (Show)

data FeatureConfiguration = FeatureConfiguration {
	fcRoot :: Root
} deriving (Show)
\end{code}

A feature is either a concrete description of a product capability (with a 
name, type, children and so on); or an error representation of a feature, which 
is usefull for reporting some kinds of errors. 

\begin{code}
data Feature = Feature {
	fId :: Id, 
	fName :: Name, 
	fType :: FeatureType,
	groupType :: GroupType,
	children :: Children,
	properties :: Properties
 } | FeatureError    
\end{code}

Bellow we present the definition of the global constraints, which can be 
either an \texttt{implies} constraint or an \texttt{iff} constraint.  
The \texttt{implies} constraint states that if the \texttt{LHS feature expression} holds as True, 
the related \texttt{RHS expression}  must also be evaluated as True in a 
feature configuration. On the other hand, the \texttt{iff} constraint states that 
the \texttt{LHS feature expression} holds as true if, and only if, the related RHS is evalueated as true
for a given feature configuration. 

\begin{code}

data ConstraintType = Implies | Iff 
 deriving (Show)

data Constraint = Constraint { 
 	constraintType :: ConstraintType, 
        constraintLHSExp :: FeatureExpression, 
	constraintRHSExp :: FeatureExpression
} deriving (Show) 
\end{code}

Feature expressions are written in propositional formula, with the NOT, AND, and OR connectors. 
Expressions are usefull for specifying constraints, as state above, but are also usefull in the 
definition of the configuration knowledge. In this later case, they are necessary for 
dealing with some types of feature interactions. 

\begin{code}
data FeatureExpression = 
 ConstantExpression Bool |
 FeatureRef Id | 
 Not FeatureExpression | 
 And FeatureExpression FeatureExpression |
 Or FeatureExpression FeatureExpression 

expTrue = ConstantExpression True
expFalse = ConstantExpression False
\end{code}

\subsection{Type Checker for feature models}

The type checker for a feature model iterates over the 
features (starting from the root element) and checks if each 
feature is well typed. Additionally, 

\begin{itemize} 
\item The global constraints of the feature model must be well typed; and 
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
  cRootFeature  = featureTC (fmRoot fm) 
  cConstraints  = constraintsTC fm
  cDuplications = noDuplicationsTC fm 
  cSatisfiable  = if (isSatisfiable fm) then [] else ["Feature model is not satisfiable."]
  errors = cRootFeature ++ cConstraints ++ cDuplications ++ cSatisfiable 
\end{code}


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


Just some syntactic sugars used for:
\begin{itemize}
 \item converting $p \Rightarrow q$ into $\lnot p \lor q$
 \item generating an and expression from a list of expressions
 \item generating an or expression from a list of expressions  
\end{itemize}

\begin{code}  

(|=>) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 |=> e2 = Or (Not e1) e2

foldAnd xs = simplifyExpression (foldr And (expTrue) xs)
foldOr xs  = simplifyExpression (foldr Or  (expFalse) xs)

ref :: Feature -> FeatureExpression
ref f = FeatureRef (fId f)

\end{code}

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


\subsection{Type checker for feature configurations}

The \texttt{eval} function checks if a feature configuration is 
refined by a feature expression. This mean that the feature configuration 
should comply to the constraints represented in the feature expression. 

\begin{code}
eval :: FeatureConfiguration -> FeatureExpression -> Bool
eval config (FeatureRef ref) = featureExists ref (fcRoot config)
eval config (Not e) = not (eval config e)
eval config (And e1 e2) = (eval config e1) && (eval config e2)
eval config (Or e1 e2) = (eval config e1) || (eval config e2)
eval _ (ConstantExpression e) = e
\end{code}

Just a set of auxiliary functions. 

\begin{code}
-- Property related functions
propertyName :: Property -> String
propertyName (name, value) = name

propertyValue :: Property -> String
propertyValue (name, value) = value
\end{code}

The \texttt{featureOptions} function retrieves the selected options 
of an \emph{alternative feature} or \emph{or feature}. An error is 
reported if this function is called with a \emph{basic feature} as 
argument. Besides that, the featureOptionsValues function retrieves 
a string representation of the selected values of an \emph{alternative feature} 
or \emph{or feature}.

\begin{code}
featureOptions :: Feature -> Children 
featureOptions feature = 
 if groupType feature == BasicFeature 
  then error "The function featureOptions can not be applied to basic features"
  else children feature 

featureOptionsValues :: Feature -> [String]
featureOptionsValues feature = 
 [fName x | x <- (featureOptions feature)]
\end{code}

If the application developer is interested in a property of the 
selected options, the \texttt{featureOptionsPropertyValue} function can 
be used, instead of the two functions shown immediately above. Again, 
this function expect a basic feature as the first argument. Elsewhere, 
an error is reported.  

\begin{code}
featureOptionsPropertyValue :: Feature -> String -> ValueList
featureOptionsPropertyValue feature property = 
  [featurePropertyValue (properties x) property | x <- featureOptions feature]  

-- 
-- just an auxiliarly function.
--
featurePropertyValue ::  Properties -> String -> String
featurePropertyValue [] property = error ("The property " ++ property ++ " is not defined") 
featurePropertyValue (x:xs) property = 
 if ((propertyName x) == property) 
  then propertyValue x  
  else featurePropertyValue xs property 

\end{code}

Now, we define several functions for checking if a 
feature configuration is a valida feature model instance. First of 
all, we have to check if a feature f1 is present in a feature tree starting 
from f2. Bellow, it is also define a function for finding a feature f1 in a 
feature tree f2. 

\begin{code}
-- just an auxiliarly function for traversing all features
plainFeature :: Feature -> FeatureList
plainFeature f = f : plainFeature' (children f)
 where 
  plainFeature' [] = [] 
  plainFeature' (x:xs) = (plainFeature x) ++ (plainFeature' xs)

 
featureExists :: Id -> Feature -> Bool 
featureExists ref feature = elem ref [fId x | x<- (plainFeature feature)]

 
findFeature :: Feature -> Feature -> Feature
findFeature f1 f2 = findFeature' f1 (plainFeature f2)  
 where 
  findFeature' f1 [] = FeatureError  
  findFeature' f1 (x:xs) = if (f1 == x) then x else findFeature' f1 xs
\end{code}

This function checks if all children of a feature f1 (defined, 
for instance, in a product configuration) are also defined as 
children of a feature f2 (defined, for instance, in a feature model).

\begin{code} 
allChildrenExists :: Feature -> Feature -> Bool 
allChildrenExists f1 f2 = 
 foldr (&&) True [elem x (children f2) | x <- (children f1)]

findChildFeature :: Feature -> Feature -> Feature
findChildFeature f1 f2 = findChildFeature' f1 (children f2)
 where 
  findChildFeature' _ [] = FeatureError 
  findChildFeature' f1 (x:xs) = if (f1 == x) then x else findChildFeature' f1 xs 

findFeatureInList :: Feature -> FeatureList -> Feature
findFeatureInList f [] = FeatureError 
findFeatureInList f (x:xs) = if (f == x) then x else findFeatureInList f xs

\end{code}
 
The \texttt{validIntacen} is the top most function for 
checking if a feature configuration (fc) is a valid instance 
of a feature model (fm). In order to do that, it is necessary to 
check if the root feature of those models are equal. Then, 
each constraint of the feature model should be considered by 
the configuration knowledge. 
 
\begin{code}
validInstance :: FeatureModel -> FeatureConfiguration -> ErrorList
validInstance fm fc = 
 let f1 = fmRoot fm
     f2 = fcRoot fc
 in 
  if (f1 == f2) then checkFeatures f1 f2
  else ["The root elements must be the same"]

-- 
-- Check if a given feature (the second parameter) 
-- is adherent with a feature present in a feature model.
-- This checking is based on the group type of the feature.
--
checkFeatures :: Feature -> Feature -> ErrorList
checkFeatures FeatureError fc = [("Feature " ++ (fId fc) ++ " not expected")]
checkFeatures fm  FeatureError = [("Expecting feature " ++ (fId fm))]
checkFeatures fm fc = 
 case (groupType fm) of 
   BasicFeature -> checkBasicFeature fm fc
   AlternativeFeature -> checkAlternativeFeature fm fc  
   OrFeature -> checkOrFeature fm fc

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

existError :: ErrorList -> Bool
existError [] = False
existError (x:xs) = True

-- 
-- Eq instance definition is (or are)
-- placede in this point.
--
instance Eq Feature where 
 Feature id1 _ _ _ _ _  == Feature id2  _ _ _ _ _ = id1 == id2
 FeatureError == _ = False
 _ == FeatureError = False 
 
instance Eq FeatureExpression where 
 FeatureRef id1 == FeatureRef id2 = id1 == id2
 FeatureRef id1 == _ = False  

 ConstantExpression e1 == ConstantExpression e2 = e1 == e2
 ConstantExpression e1 == _ = False

 Not fExp1 == Not fExp2 = fExp1 == fExp2 
 Not fExp1 == _ = False

 And fExp1 fExp2 == And fExp3 fExp4 = 
  ((fExp1 == fExp3) && (fExp2 == fExp4)) || ((fExp1 == fExp4) && (fExp2 == fExp3))
 And fExp1 fExp2 == _ = False
 
 Or fExp1 fExp2 == Or fExp3 fExp4 = 
  ((fExp1 == fExp3) && (fExp2 == fExp4)) || ((fExp1 == fExp4) && (fExp2 == fExp3))
 Or fExp1 fExp2 == _ = False

-- 
-- Show instance definition are 
-- placed in this point.
--
instance Show Feature where 
 show (Feature i1 n1 t1 g1 [] _) = i1   
 show (Feature i1 n1 t1 g1 (x:xs) _) = i1 ++ show [y | y<-(x:xs)]   
 show FeatureError = "Feature error" 

instance Show FeatureExpression where
  show (FeatureRef id1) = show id1
  show (And exp1 exp2) = "And (" ++ (show exp1) ++ ", " ++ (show exp2) ++ ")"
  show (Or exp1 exp2) = "Or (" ++ (show exp1) ++ ", " ++ (show exp2) ++ ")"
  show (Not exp1) = "Not (" ++ (show exp1)  ++ ")"
  show (ConstantExpression True) = "True"
  show (ConstantExpression False) = "False" 

-- and expression simplifcations
simplifyExpression :: FeatureExpression -> FeatureExpression
simplifyExpression (And e1 e2) = simplifyAnd e1 e2 
simplifyExpression (Or e1 e2)  = simplifyOr e1 e2
simplifyExpression (Not e)     = simplifyNot e
simplifyExpression (FeatureRef f)        = FeatureRef f 
simplifyExpression (ConstantExpression b) = ConstantExpression b

simplifyAnd :: FeatureExpression -> FeatureExpression -> FeatureExpression
simplifyAnd e1 e2  
 | (e1 == expFalse) || (e2 == expFalse) = expFalse
 | e1 == expTrue = simplifyExpression e2
 | e2 == expTrue = simplifyExpression e1
 | otherwise = And (simplifyExpression e1) (simplifyExpression e2)


simplifyOr :: FeatureExpression -> FeatureExpression -> FeatureExpression
simplifyOr e1 e2  
 | (e1 == expTrue) || (e2 == expTrue) = expTrue
 | e1 == expFalse = simplifyExpression e2
 | e2 == expFalse = simplifyExpression e1
 | otherwise = Or (simplifyExpression e1) (simplifyExpression e2)

simplifyNot :: FeatureExpression -> FeatureExpression 
simplifyNot e 
 | e == expTrue = expFalse
 | e == expFalse = expTrue
 | otherwise = Not (simplifyExpression e)

\end{code}

\end{document}
