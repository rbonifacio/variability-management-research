\section{Feature Model to Propositional Logic Tests}

\begin{code}
module FeatureModelDummy where

import FeatureModel.FeatureModel
import Test.HUnit

(|&&) :: FeatureExpression -> FeatureExpression -> FeatureExpression
exp1 |&& exp2 = And exp1 exp2
 
a = Feature {
 fId   = "A", 
 fName = "A",
 fType = Mandatory,
 groupType = BasicFeature,
 children =  [],
 properties = [] 
}

b = Feature {
 fId   = "B", 
 fName = "B",
 fType = Mandatory,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

c = Feature {
 fId   = "C", 
 fName = "C",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}


d = Feature {
 fId   = "D", 
 fName = "D",
 fType = Mandatory,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

e1 = Feature {
 fId   = "E1", 
 fName = "E1",
 fType = Mandatory,
 groupType = AlternativeFeature,
 children = [f1],
 properties = [] 
}

e2 = Feature {
 fId   = "E1", 
 fName = "E1",
 fType = Mandatory,
 groupType = AlternativeFeature,
 children = [f1,f2],
 properties = [] 
}

f1 = Feature {
 fId   = "F1", 
 fName = "F1",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

f2 = Feature {
 fId   = "F2", 
 fName = "F2",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

a1 = a { children = [b]}
a2 = a { children = [b,c]}
a3 = a { children = [b,c,d]}
a4 = a { children = [b,c,d,e1]}
a5 = a { children = [b,c,d,e2]}


fm01 = FeatureModel {
 fmRoot = a,
 fmConstraints = []
}

fm02 = fm01 { fmRoot = a1 }

fm03 = fm01 { fmRoot = a2 }

fm04 = fm01 { fmRoot = a3 }

fm05 = fm01 { fmRoot = a4}

test1 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with just the root feature"
                  (ref a) 
                  (fmToCNFExpression fm01))

test2 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b"
                  (And (ref a) ( (ref a) |=> (ref b) )) 
                  (fmToCNFExpression fm02))

test3 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b,c"
                  (And (ref a) ( (ref a) |=> (ref b) )) 
                  (fmToCNFExpression fm03))

test4 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b,c,d"
                  (toCNFExpression (And (ref a) (And ( (ref a) |=> (ref b) ) ( (ref a) |=> (ref d) ) ) ) )  
                  (fmToCNFExpression fm04))

test5 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b,c,d,e1"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a) |=> (ref b) ) 
                                        (And ( (ref a) |=> (ref d) ) 
                                        (And ( (ref a) |=> (ref e1) ) ( (ref e1) |=> (ref f1) ) ) ) ) ))
                  (fmToCNFExpression fm05))

allTests = TestList [TestLabel "test1" test1, 
                     TestLabel "test2" test2,
                     TestLabel "test3" test3,
                     TestLabel "test4" test4,
                     TestLabel "test5" test5]

\end{code}
