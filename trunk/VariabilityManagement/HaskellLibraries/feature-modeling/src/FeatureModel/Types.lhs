
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
--| This modulde defines the main concepts related 
--  to feature modeling.
module FeatureModel.Types where 

import List
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
-- * data types
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

Just some syntactic sugars used for:
\begin{itemize}
 \item converting $p \Rightarrow q$ into $\lnot p \lor q$
 \item generating an And expression from a list of expressions
 \item generating an Or expression from a list of expressions 
 \item generating an FeatureRef expression from a feature 
\end{itemize}

\begin{code}  

(|=>) :: FeatureExpression -> FeatureExpression -> FeatureExpression
e1 |=> e2 = Or (Not e1) e2

foldAnd xs = simplifyExpression (foldr And (expTrue) xs)
foldOr xs  = simplifyExpression (foldr Or  (expFalse) xs)

ref :: Feature -> FeatureExpression
ref f = FeatureRef (fId f)

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