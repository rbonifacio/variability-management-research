module AbstractModel where

class Model m where 
 emptyModel :: m -> m
 
type Model2Model model = (model -> model -> model)