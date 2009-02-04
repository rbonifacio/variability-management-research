\section{Unit tests}

This section presents the unit tests performed 
for the Feature Model library. A feature model 
for the automobile domain is used for as the basis 
for almost all tests. 

\begin{code}
module FeatureModelTests where 

import FeatureModel.FeatureModel

import Test.HUnit

--
-- the root element of our car feature model
-- this is the feature model under test
--
fmCarro = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [fmMotorizacao, fmDirecaoHidraulica], 
 properties = []
}

fmCarroError = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [fmMotorizacaoError, fmDirecaoHidraulica], 
 properties = []
}

fmMotorizacao = Feature {
 fId = "Motorizacao", 
 fName = "Motorizacao do automovel",
 fType = Mandatory,
 groupType = AlternativeFeature, 
 children = [fmMotorizacaoEconomica, fmMotorizacaoAltaPotencia, fmMotorizacaoTurbo], 
 properties = []
}

fmMotorizacaoError = Feature {
 fId = "Motorizacao", 
 fName = "Motorizacao do automovel",
 fType = Mandatory,
 groupType = AlternativeFeature, 
 children = [fmMotorizacaoEconomica, fmMotorizacaoEconomica], 
 properties = []
}

fcMotorizacao1 = Feature {
 fId = "Motorizacao", 
 fName = "Motorizacao do automovel",
 fType = Mandatory,
 groupType = AlternativeFeature, 
 children = [fmMotorizacaoEconomica], 
 properties = []
}


fcMotorizacao2 = Feature {
 fId = "Motorizacao", 
 fName = "Motorizacao do automovel",
 fType = Mandatory,
 groupType = AlternativeFeature, 
 children = [fmMotorizacaoAltaPotencia], 
 properties = []
}

fcMotorizacao3 = Feature {
 fId = "Motorizacao", 
 fName = "Motorizacao do automovel",
 fType = Mandatory,
 groupType = AlternativeFeature, 
 children = [fmMotorizacaoEconomica, fmMotorizacaoAltaPotencia], 
 properties = []
}


fcMotorizacao4 = Feature {
 fId = "Motorizacao", 
 fName = "Motorizacao do automovel",
 fType = Mandatory,
 groupType = AlternativeFeature, 
 children = [fmDirecaoHidraulica], 
 properties = []
}


fmMotorizacaoEconomica = Feature { 
 fId = "MotorizacaoBasica", 
 fName = "Motorizacao basica, ou economica", 
 fType = Optional,
 groupType = BasicFeature, 
 children = [], 
 properties = [] 
}

fmMotorizacaoAltaPotencia = Feature { 
 fId = "MotorizacaoAltaPotencia", 
 fName = "Motorizacao alta potencia, para um alto perfil de aquisicao", 
 fType = Optional, 
 groupType = BasicFeature, 
 children = [], 
 properties = [] 
}


fmMotorizacaoTurbo = Feature { 
 fId = "MotorizacaoTurbo", 
 fName = "Motorizacao turbo", 
 fType = Optional, 
 groupType = BasicFeature, 
 children = [], 
 properties = [] 
}

fmDirecaoHidraulica = Feature { 
 fId = "DirecaoHidraulica", 
 fName = "Direcao hidraulica", 
 fType = Optional,
 groupType = BasicFeature, 
 children = [], 
 properties = [] 
}

fmConstraint1 :: Constraint
fmConstraint1 = Constraint {
  constraintType = Implies,
  constraintLHSExp = FeatureRef (fId fmDirecaoHidraulica),
  constraintRHSExp = FeatureRef (fId fmMotorizacaoAltaPotencia)
}

fmConstraint2 :: Constraint
fmConstraint2 = Constraint {
  constraintType = Implies,
  constraintLHSExp = FeatureRef (fId fmDirecaoHidraulica),
  constraintRHSExp = FeatureRef (fId fcDummy)
}

fmConstraint3 :: Constraint
fmConstraint3 = Constraint {
  constraintType = Implies,
  constraintLHSExp = FeatureRef (fId fcDummy),
  constraintRHSExp = FeatureRef (fId fmMotorizacaoAltaPotencia)
}

fmAlternativeFeature = Feature { 
 fId = "InvalidAlternativeFeature", 
 fName = "InvalidAlternativeFeature", 
 fType = Optional,
 groupType = AlternativeFeature, 
 children = [], 
 properties = [] 
}

fmOrFeature = Feature { 
 fId = "InvalidOrFeature", 
 fName = "InvalidOrFeature", 
 fType = Optional,
 groupType = OrFeature, 
 children = [], 
 properties = [] 
}


fcDummy = Feature { 
 fId = "Dummy", 
 fName = "Dummy", 
 fType = Optional,
 groupType = BasicFeature, 
 children = [], 
 properties = [] 
}

fcCarro1 = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [], 
 properties = []
}

fcCarro2 = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [fcMotorizacao1, fmDirecaoHidraulica], 
 properties = []
}


fcCarro3 = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [fcMotorizacao3], 
 properties = []
}

fcCarro4 = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [fcMotorizacao4], 
 properties = []
}


fcCarro5 = Feature {
 fId = "Carro", 
 fName = "The root feature of the Car feature model", 
 fType = Mandatory,
 groupType = BasicFeature,
 children = [fcMotorizacao1, fcDummy], 
 properties = []
}

-- 
-- invalid feature configutaion, since 
-- there is no option for the engine 
-- 
fc1 = FeatureConfiguration {
  fcRoot = fcCarro1
} 

fc2 = FeatureConfiguration {
  fcRoot = fcCarro2	
}

fc3 = FeatureConfiguration {
  fcRoot = fcCarro3	
}

fc4 = FeatureConfiguration {
  fcRoot = fcCarro4 	
}

fc5 = FeatureConfiguration {
  fcRoot = fcCarro5 	
}

fm1 = FeatureModel {
  fmRoot = fmCarro,
  fmConstraints = [fmConstraint1]
}

fm2 = FeatureModel {
  fmRoot = FeatureError
}

fm3 = FeatureModel {
  fmRoot = fmCarroError
}


testFC1 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc1)))
testFC2 = TestCase (assertEqual "checkFeatures should hold" False (existError (validInstance fm1 fc2)))
testFC3 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc3)))
testFC4 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc4)))
testFC5 = TestCase (assertEqual "checkFeatures should not hold" True (existError (validInstance fm1 fc5)))

\end{code}
