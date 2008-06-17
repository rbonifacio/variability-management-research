{- 
 -- 
 -- This module defines a parametric 
 -- Environment data type. Such data type 
 -- can be used to relate Ids with features, variables, 
 -- and so on.
 -- 
 -- The main function is hash (environment, key) that 
 -- returns a concept that was previously mapped to 
 -- the key.
 --
 -}

module Environment where

type Key = String

data Environment a = Environment[EnvItem a]
data EnvItem a = EnvItem (Key, a)

-- return the EnvItem key 
envKey :: EnvItem a -> String
envKey (EnvItem (key, value)) = key

-- return the EnvItem value
envValue :: EnvItem a -> a
envValue (EnvItem (key, value)) = value

-- 
-- hash (env, key)
--
-- Description: 
--
-- Return the value of the item present in the 
-- environment (env) that has the key equals to 
-- the second parameter (key)
--
hash :: Environment a -> String -> a
hash (Environment []) key = error ("The item " ++ key ++ "is not mapped to the environment")
hash (Environment (x:xs)) key = 
 if (envKey x) == key
  then envValue x 
  else hash (Environment xs) key

-- item1 = EnvItem ("1", "Primeiro item")
-- item2 = EnvItem ("2", "Segundo item")
-- env1 = Environment [item1, item2]
-- env2 = Environment []
