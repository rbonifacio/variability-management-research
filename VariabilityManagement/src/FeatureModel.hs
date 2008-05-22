{-
   
   FeatureModel.hs


   This source code define the main concepts (types like 
   feature model, features, restrictions, and so on) related 
   with feature modeling context. Also, this code 
   implement some functions that represent the weaving 
   process of some requirement variability mechanisms.

   Author: Rodrigo Bonifacio 
	
-}
module FeatureModel where 

import BasicTypes 
import List

type Root         = Feature
type Parent       = Feature
type FeatureList  = [Feature]
type Children     = FeatureList
type Property     = (String, String) 
type ValidInstanceResult = (Bool, String)
type Properties   = [Property]
type GroupType    = Integer
type FeatureType  = Integer
type ValueList	  = [String]
type ErrorList 	  = [String]

-- The feature type "menemonics"
optional = 0
mandatory = 1


-- The group types "mnemonics"
--
-- Basic features do not define a group. They can
-- have children, however, there is not an explicit
-- semantic relating them (the children). 
-- 
-- The other types of feature (alternative and orFeature) 
-- must have children, and there is a semantic relating them. 
-- An alternative feature requires that exactly one child must be 
-- selected, if it is present. 
-- 
-- An orFeature enables at least one child to be selected, 
-- if the orFeature was selected.
--
basicFeature = 0
alternativeFeature = 1
orFeature = 2	

-- type Restrictions = [Restriction]

data FeatureModel = FeatureModel Root  -- Restrictions 
 deriving (Show)
data FeatureConfiguration = FeatureConfiguration Root
 deriving (Show)
data Feature = 
 Feature Id Name FeatureType GroupType Children Properties | 
 FeatureError  


-- data Restriction = ImpliesFeature | NotFeatures

data ImpliesFeature = ImpliesFeature FeatureExpression FeatureList
data NotFeatures = NotFeatures FeatureExpression FeatureList

data FeatureExpression = 
 FeatureRef Id | 
 NotExpression FeatureExpression | 
 AndExpression FeatureExpression FeatureExpression |
 OrExpression FeatureExpression FeatureExpression 


-- 
-- This function is responsible for the evaluation of 
-- feature expressions. 
--
eval :: FeatureConfiguration -> FeatureExpression -> Bool
eval (FeatureConfiguration r) (FeatureRef id) = featureExists [r] id
eval config (NotExpression e) = not (eval config e)
eval config (AndExpression e1 e2) = (eval config e1) && (eval config e2)
eval config (OrExpression e1 e2) = (eval config e1) || (eval config e2)

-- FeatureModel (and Feature) related functions
fmRoot :: FeatureModel -> Feature
fmRoot (FeatureModel root) = root

fcRoot :: FeatureConfiguration -> Feature
fcRoot (FeatureConfiguration root) = root

fId :: Feature -> String
fId (Feature id _ _ _ _ _) = id

fName :: Feature -> String
fName (Feature _ name _ _ _ _) = name

fType :: Feature -> FeatureType
fType (Feature _ _ t _ _ _) = t

groupType :: Feature -> GroupType
groupType (Feature _ _ _ group _ _) = group 

children :: Feature -> Children
children (Feature _ _ _ _ c _) = c

findChild :: FeatureList -> String -> Feature
findChild [] s = FeatureError    
findChild (x:xs) s =  
 if (fId x == s) 
  then x
  else findChild xs s 

properties :: Feature -> Properties
properties (Feature _ _ _ _ _ properties) = properties

-- existis :: FeatureModel -> Feature -> Bool

-- Property related functions
propertyName :: Property -> String
propertyName (name, value) = name

propertyValue :: Property -> String
propertyValue (name, value) = value

-- featureOptions(feature)
--
-- Return the possible options related with an alternativeFeature 
-- or orFeature. An error  is reported if this function is called 
-- with a basicFeature as the feature parameter. 
--
featureOptions :: Feature -> FeatureList
featureOptions (Feature _ _ _ group children _) = 
 if group == basicFeature 
  then error "The function featureOptions can not be applied to basic features"
  else children

-- 
-- optionValues(FeatureList) 
--
-- Description: 
-- Return a string representation of a group features.
--
optionValues :: FeatureList -> [String]
optionValues features = 
 [fName x | x <- features]

-- 
-- featureOptionPropertyValues(feature, name) 
-- 
-- Description:
--
-- Return the valid property values related with an 
-- alternativeFeature or orFeature. An error is reported if this
-- this function is called with an basicFeature as the feature 
-- parameter or the name of the property does not exists in at least one 
-- of the feature childs.
--
-- Usage sample: featureOptionValues size "size"
--
featureOptionPropertyValues :: Feature -> String -> ValueList
featureOptionPropertyValues (Feature _ _ _ group children _) name = 
 if group == basicFeature
  then error "The function featureOptions can not be applied to basic features"
  else [featurePropertyValue (properties x) name | x <- children]  

-- 
-- featurePropertyValue(properties, name)
-- 
-- Description: 
-- Extract the value of a property (named as the name parameter) 
-- from the list of properties (properties parameter).
--
featurePropertyValue ::  Properties -> String -> String
featurePropertyValue [] name = error ("The property " ++ name ++ " is not defined") 
featurePropertyValue (x:xs) name = 
 if ((propertyName x) == name) 
  then propertyValue x  
  else featurePropertyValue xs name
 
-- 
-- Check if a feature with id "id" exists in a list of features ([]/x:xs) or 
-- a subfeature of this list of feature.
-- This function is useful for identifying if a feature is 
-- present in a feature configuration. For this, we can 
-- call this function as featureExists ([fcRoot fc]) f, where: 
-- * fc is a feature configuration
-- * fcRoot returns the root element of fc
-- * f is the desired feature
--
featureExists :: FeatureList -> Id -> Bool
featureExists [] _ = False
featureExists (x:xs) id1 = 
 if (fId x) == id1 then True
 else (featureExists (children x) id1) || (featureExists xs id1)

allFeatureExists :: FeatureList -> FeatureList -> Bool
allFeatureExists base [] = False
allFeatureExists base (x:xs) = and [featureExists base (fId(y)) | y <- (x:xs)] 

--
-- Check if a feature configuration is a valid 
-- feature model instance. They must have the same 
-- root, and a feature check must be performed between 
-- both (the feature model root and the feature configuration 
-- root)
-- 

validInstance :: FeatureModel -> FeatureConfiguration -> ErrorList
validInstance (FeatureModel f1) (FeatureConfiguration f2) = 
 if  f1 == f2
  then checkFeatures f1 f2
  else ["The root elements must be the same"]



-- 
-- Check if a given feature (the second parameter) 
-- is adherent with a feature present in a feature model.
-- This checking is based on the group type of the feature.
--

checkFeatures :: Feature -> Feature -> ErrorList
checkFeatures (Feature i1 n1 t1 g1 c1 p1) FeatureError = [("Expecting feature " ++ i1)]
checkFeatures (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2) = 
 if g1 == basicFeature 
  then checkBasicFeature (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2)
  else if g1 == alternativeFeature 
   then checkAlternativeFeature (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2)
   else checkOrFeature (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2)



-- case g1 of
--  basicFeature -> checkBasicFeature (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2)
--  alternativeFeature -> checkAlternativeFeature (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2)
--  orFeature -> checkOrFeature (Feature i1 n1 t1 g1 c1 p1) (Feature i2 n2 t2 g2 c2 p2)



--
-- Check if a feature (the second parameter "f2") is adherent
-- with a basic feature (the first parameter "f1") of the feature 
-- model. A basic feature can have zero or more childs. 
-- Case a child of f1 is mandatory, it must be selected as a child of f2.
-- Case a child of f1 is optional, it can be selected as a child of f2.
-- The existence of a child in both features requires that they must be 
-- checekd.
--
-- Note: We must iterate over the f1 children
--
-- try: [x+y | x <- [1,3,5], y <- [1,3, 4], x==y]

checkBasicFeature :: Feature -> Feature -> ErrorList
checkBasicFeature (Feature _ _ _ _ [] _) _ = []
checkBasicFeature (Feature i1 n1 t1 g1 (x:xs) p1) (Feature i2 n2 t2 g2 c2 p2) = 
 (
  if ((fType x) == mandatory) 
   then checkFeatures x (findChild c2 (fId x))
   else 
    if findChild c2 (fId x) == FeatureError 
     then []
     else checkFeatures x (findChild c2 (fId x))
 ) ++  checkBasicFeature (Feature i1 n1 t1 g1 xs p1) (Feature i2 n2 t2 g2 c2 p2)



--
-- Check if a feature (the second parameter "f2") is adherent
-- with an alternative feature (the first parameter "f1") of the feature 
-- model. An alternative feature requires only the selection of one child, 
-- that must be checked.
--

checkAlternativeFeature :: Feature -> Feature -> ErrorList
checkAlternativeFeature (Feature i1 _ _ _ [] _) (Feature _ _ _ _ _ _) = [("Expecting one child selected for feature " ++ i1)]
checkAlternativeFeature (Feature i1 _ _ _ (x:xs) _) (Feature _ _ _ _ [] _) = [("Expecting one child selected for feature " ++ i1)]
checkAlternativeFeature (Feature i1 _ _ _ (x:xs) _) (Feature _ _ _ _ (y:ys) _) = 
 if length (y:ys) == 1 
  then 
   if findChild (x:xs) (fId y) == FeatureError 
    then [("Feature " ++ (fId y) ++ " is invalid for feture " ++ i1)]
    else checkFeatures (findChild (x:xs) (fId y)) y
  else [("Exactly one child must be selected for feture " ++ i1)]

--
-- Check if a feature (the second parameter "f2") is adherent
-- with an "or" feature (the first parameter "f1") of the feature 
-- model. An "or" feature requires the selection of at least one child, 
-- that must be checked.
-- 
-- Note: We must iterate over the childs of f1
--
checkOrFeature :: Feature -> Feature -> ErrorList
checkOrFeature (Feature i1 n1 _ _ [] _) _ = []
checkOrFeature (Feature i1 n1 _ _ (x:xs) _) (Feature _ _ _ _ [] _) = [ ("At least one child of " ++ i1 ++ " must be selected") ]
checkOrFeature (Feature i1 n1 t1 g1 (x:xs) p1) (Feature i2 n2 t2 g2 (y:ys) p2) = 
 (
  if findChild (y:ys) (fId x) == FeatureError 
   then []
   else checkFeatures x (findChild (y:ys) (fId x)) 
 ) ++ (checkOrFeature (Feature i1 n1 t1 g1 xs p1 ) (Feature i2 n2 t2 g2 (y:ys) p2))	


existError :: ErrorList -> Bool
existError [] = False
existError (x:xs) = True

-- 
-- Eq instance definition is (or are)
-- placede in this point.
--
instance Eq Feature where 
 Feature id1 _ _ _ _ _  == Feature id2  _ _ _ _ _ = id1 == id2
 FeatureError == FeatureError = True
 Feature _ _ _ _ _ _  == FeatureError = False
 FeatureError == Feature _ _ _ _ _ _ = False

-- 
-- Show instance definition are 
-- placed in this point.
--
instance Show Feature where 
 show (Feature i1 n1 t1 g1 [] _) = i1   
 show (Feature i1 n1 t1 g1 (x:xs) _) = i1 ++ show [y | y<-(x:xs)]   
 show (FeatureError) = "Feature error"   

