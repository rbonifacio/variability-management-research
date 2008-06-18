module AbstractModel where

class AbstractModel m where 
 emptyModel :: m -> m
 
type Model2Model model = (model -> model -> model)


