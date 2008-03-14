module QuickCheckSample where 

import ShowFunctions
import QuickCheck

prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Integer]