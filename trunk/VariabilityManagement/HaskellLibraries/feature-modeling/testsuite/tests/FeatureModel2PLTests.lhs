\section{Feature Model to Propositional Logic Tests}

This section presents several unit tests for the functions 
related to the mapping of feature models to propositional 
logic. 

\begin{code}
module FeatureModToPropositionalLogicTests where

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
 fId   = "E2", 
 fName = "E2",
 fType = Mandatory,
 groupType = AlternativeFeature,
 children = [f1,f2],
 properties = [] 
}

e3 = Feature {
 fId   = "E3", 
 fName = "E3",
 fType = Mandatory,
 groupType = AlternativeFeature,
 children = [f1,f2,f3],
 properties = [] 
}

e4 = Feature {
 fId   = "E4", 
 fName = "E4",
 fType = Mandatory,
 groupType = AlternativeFeature,
 children = [f1,f2,f31],
 properties = [] 
}

h1 = Feature {
 fId = "H1",
 fName = "H1",
 fType = Optional,
 groupType = OrFeature,
 children = [i1],
 properties = [] 
}

h2 = Feature {
 fId = "H2",
 fName = "H2",
 fType = Optional,
 groupType = OrFeature,
 children = [i1, i2, i3],
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


f3 = Feature {
 fId   = "F3", 
 fName = "F3",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

f31 = Feature {
 fId   = "F31", 
 fName = "F31",
 fType = Optional,
 groupType = BasicFeature,
 children = [g],
 properties = [] 
}

g = Feature {
 fId   = "G", 
 fName = "G",
 fType = Mandatory,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

i1 = Feature {
 fId   = "I1", 
 fName = "I1",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

i2 = Feature {
 fId   = "I2", 
 fName = "I2",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

i3 = Feature {
 fId   = "I3", 
 fName = "I3",
 fType = Optional,
 groupType = BasicFeature,
 children = [],
 properties = [] 
}

cnt01 = Constraint {
 constraintType = Implies,
 constraintLHSExp = (ref c),
 constraintRHSExp = (ref i3)
}

cnt02 = Constraint {
 constraintType = Implies, 
 constraintLHSExp = (ref b),
 constraintRHSExp = Not (ref a)
}

a1 = a { children = [b]}
a2 = a { children = [b,c]}
a3 = a { children = [b,c,d]}
a4 = a { children = [b,c,d,e1]}
a5 = a { children = [b,c,d,e2]}
a6 = a { children = [b,c,d,e3]}
a7 = a { children = [b,c,d,e4]}
a8 = a { children = [b, h1] }
a9 = a { children = [b, h2] }

fm01 = FeatureModel {
 fmRoot = a,
 fmConstraints = []
}

fm02 = fm01 { fmRoot = a1 }

fm03 = fm01 { fmRoot = a2 }

fm04 = fm01 { fmRoot = a3 }

fm05 = fm01 { fmRoot = a4 }

fm06 = fm01 { fmRoot = a5 }

fm07 = fm01 { fmRoot = a6 }

fm08 = fm01 { fmRoot = a7 } 

fm09 = fm01 { fmRoot = a8 }

fm10 = fm01 { fmRoot = a9 }

fm11 = fm10 { fmConstraints = [cnt01] }

fm12 = fm04 { fmConstraints = [cnt02] }

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

test6 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b,c,d,e1,e2"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a)  |=> (ref b) ) 
                                        (And ( (ref a)  |=> (ref d) ) 
                                        (And ( (ref a)  |=> (ref e2) ) ( (ref e2) |=> 
                                        (Or  (And (ref f1) (Not (ref f2))) ( And (ref f2) (Not (ref f1) ) ) ) ) ) ))
                                    ) 
                  )
                  (fmToCNFExpression fm06))

test7 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b,c,d,e1,e2,e3"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a)  |=> (ref b) ) 
                                        (And ( (ref a)  |=> (ref d) ) 
                                        (And ( (ref a)  |=> (ref e3) ) ( (ref e3) |=> 
                                        (Or  ( And (ref f1) (And (Not (ref f2)) (Not (ref f3) ) ) ) 
                                        (Or  ( And (ref f2) (And (Not (ref f1)) (Not (ref f3) ) ) ) 
                                        (    ( And (ref f3) (And (Not (ref f1)) (Not (ref f2) ) ) )) 
                                        ) ) ) ) ) )   
                  ))
                  (fmToCNFExpression fm07))

test8 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b,c,d,e1,e2,e4"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a)  |=> (ref b) ) 
                                        (And ( (ref a)  |=> (ref d) ) 
                                        (And ( (ref a)  |=> (ref e4) ) 
                                        (And ( (ref e4) |=> 
                                        (Or  ( And (ref f1) (And (Not (ref f2)) (Not (ref f31) ) ) ) 
                                        (Or  ( And (ref f2) (And (Not (ref f1)) (Not (ref f31) ) ) ) 
                                        (    ( And (ref f31) (And (Not (ref f1)) (Not (ref f2) ) ) ) ) ) ) ) 
                                        ( (ref f31) |=> (ref g) ) ) ) )   
                  )))
                  (fmToCNFExpression fm08))

test9 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b, i1"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a)  |=> (ref b) )  
                                        (    ( (ref h1) |=> (ref i1)) ))))  
                  (fmToCNFExpression fm09))

test10 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b, i1"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a)  |=> (ref b) )  
                                        (    ( (ref h2) |=> (Or (ref i1) (Or (ref i2) (ref i3) ) ) ) ) ) ) )  
                  (fmToCNFExpression fm10))

test11 = TestCase (assertEqual 
                  "for the function toCNFExpression considering a FM with the features root and b, i1, and cnt01"
                  (toCNFExpression (And (ref a) 
                                        (And ( (ref a)  |=> (ref b) )  
                                        (And    ( (ref h2) |=> (Or (ref i1) (Or (ref i2) (ref i3) ) ) ) 
                                                ( (ref c)  |=> (ref i3) ) ) ) ) )  
                  (fmToCNFExpression fm11))



allTests = TestList [TestLabel "test1"  test1, 
                     TestLabel "test2"  test2,
                     TestLabel "test3"  test3,
                     TestLabel "test4"  test4,
                     TestLabel "test5"  test5,
                     TestLabel "test6"  test6,
                     TestLabel "test7"  test7,
                     TestLabel "test8"  test8,
                     TestLabel "test9"  test9,
                     TestLabel "test10" test10,
                     TestLabel "test11" test11]

\end{code}
