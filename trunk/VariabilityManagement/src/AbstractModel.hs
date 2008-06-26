module AbstractModel where

import FeatureModel

class AbstractModel m where 
 emptyModel :: m -> m
 
-- talvez essas transformacoes precisem 
-- receber como parametro todos os modelos 
-- de entrada

-- type Model2Model model = (model -> model -> model)


data Model2Model model  =
 ConsM2MType1 { fnModel1 :: model -> model -> model} |
 ConsM2MType2 { fnModel2 :: FeatureConfiguration -> model -> model} | 
 ConsM2MType3 { fnModel3 :: FeatureConfiguration -> model -> model -> model }
 
 
