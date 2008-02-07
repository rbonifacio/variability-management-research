module BasicTypes where 

type Id = String
type Name = String
type Description = String

exists :: Eq a => a -> [a] -> Bool
exists e [] = False
exists e (x:xs) = 
 if e == x 
  then True 
  else exists e xs
  
existAll :: Eq a => [a] -> [a] -> Bool
existAll [] _ = True
existAll (x:xs) [] = False
existAll (x:xs) (y:ys) = (exists x (y:ys)) && (existAll xs (y:ys))  