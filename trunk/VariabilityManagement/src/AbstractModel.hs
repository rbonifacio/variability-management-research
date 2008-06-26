module AbstractModel where

import FeatureModel

class AbstractModel m where 
 emptyModel :: m -> m
 
-- talvez essas transformacoes precisem 
-- receber como parametro todos os modelos 
-- de entrada

type Model2Model model = (model -> model -> model)

model1 = 1
model2 = 2
model3 = 3

data M2M model = 
 M2M1 { fnModel1 :: model -> model -> model} |
 M2M2 { fnModel2 :: FeatureConfiguration -> model -> model} | 
 M2M3 { fnModel3 :: FeatureConfiguration -> model -> model -> model }
 
 
typeOfModel :: M2M model -> Integer
typeOfModel (M2M1 _) = model1
typeOfModel (M2M2 _) = model2
typeOfModel (M2M3 _) = model3