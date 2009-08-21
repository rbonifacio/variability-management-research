\section{Unit tests}

This section presents the unit tests performed 
for the Feature Model library. A feature model 
for the automobile domain is used as the basis 
for almost all tests. 

\begin{code}
module FeatureModelTests where 

import FeatureModel.Types
import FeatureModel.FMTypeChecker
import FeatureModel.FCTypeChecker

import Test.HUnit


fm = FeatureModel {
 fmTree = fmroot, 
 -- fmConstraints = c1 : c2 : ((FeatureRef "display") |=> Not (FeatureRef "wireless"))  : []
 fmConstraints = c1 : c2 : ((FeatureRef "display") |=> (FeatureRef "li-ion")) : [] 
}

fmroot = Root cellphone [treeW, treeA, treeD]
treeW  = Root wireless  [Leaf infrared, Leaf bluetooth]
treeA  = Root accucell  [Leaf liion, Leaf nimh, Leaf nica]
treeD  = Root display   [Leaf color, Leaf monochrome]



cellphone :: Feature
cellphone = Feature {
 fId = "cellphone" ,
 fName = "cellphone" ,
 fType = Mandatory , 
 groupType = BasicFeature ,
 properties = []
}

wireless :: Feature
wireless = Feature {
 fId = "wireless" ,
 fName = "wireless" , 
 fType = Optional,
 groupType = OrFeature ,
 properties = []
}

infrared :: Feature 
infrared = Feature {
 fId = "infrared" ,
 fName = "infrared" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}

bluetooth :: Feature 
bluetooth = Feature {
 fId = "bluetooth" ,
 fName = "bluetooth" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}

accucell :: Feature
accucell = Feature {
 fId = "accucell" ,
 fName = "accucell" ,
 fType = Mandatory ,
 groupType = AlternativeFeature ,
 properties = []
}

liion :: Feature 
liion = Feature {
 fId = "li-ion" ,
 fName = "li-ion" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}


nimh :: Feature 
nimh = Feature {
 fId = "ni-mh" ,
 fName = "ni-mh" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}


nica :: Feature 
nica = Feature {
 fId = "ni-ca" ,
 fName = "ni-ca" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}

display :: Feature
display = Feature {
 fId = "display" ,
 fName = "display" ,
 fType = Mandatory ,
 groupType = AlternativeFeature ,
 properties = []
}

color :: Feature 
color = Feature {
 fId = "color" ,
 fName = "color" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}

monochrome :: Feature 
monochrome = Feature {
 fId = "monochrome" ,
 fName = "monochrome" , 
 fType = Optional,
 groupType = BasicFeature ,
 properties = []
}

c1 :: FeatureExpression
c1 = (FeatureRef "bluetooth" ) |=> (FeatureRef "li-ion")

c2 :: FeatureExpression
c2 = (FeatureRef "color") |=> (FeatureRef "ni-ca")







-- expTest = Or (And (FeatureRef "a") (FeatureRef "b")) (And (Or (FeatureRef "c") (FeatureRef "d") ) (FeatureRef "e"))
-- testFC1 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc1)))
-- testFC2 = TestCase (assertEqual "checkFeatures should hold" False (existError (validInstance fm1 fc2)))
-- testFC3 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc3)))
-- testFC4 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc4)))
-- testFC5 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc5)))

\end{code}
