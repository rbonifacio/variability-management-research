module DistributionListTest where 

import Prelude hiding ( (^) )
import HUnit
import UseCaseModel

testEmpty = TestCase $ assertEqual "Lista vazia" [["1", "a"]] ("1" ^ [["a"]])

main = runTestTT testEmpty