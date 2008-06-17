module FeatureModelTests where

import FeatureModel
import Test.HUnit

exp1 = FeatureRef "fea-01"
exp2 = FeatureRef "fea-02"
exp3 = FeatureRef "fea-03"
exp4 = FeatureRef "fea-01"
nExp1 = NotExpression exp1 
nExp2 = NotExpression exp2
nExp4 = NotExpression exp4
andExp1Exp2 = AndExpression exp1 exp2
andExp2Exp1 = AndExpression exp2 exp1
andExp1Exp3 = AndExpression exp1 exp3 
andNExp1Exp2 = AndExpression nExp1 exp2
andExp2NExp1 = AndExpression exp2 nExp1

tc01 = TestCase (assertBool "Exp exp1 == exp1 must be True" (exp1 == exp1))
tc02 = TestCase (assertBool "Exp exp1 == exp2 must be False" (not (exp1 == exp2)))
tc03 = TestCase (assertBool "Exp exp1 == exp4 must be True" (exp1 == exp4))
tc04 = TestCase (assertBool "Not exp1 == Not exp1 must be True" (nExp1 == nExp1))
tc05 = TestCase (assertBool "Not exp1 == Not exp2 must be False" (not (nExp1 == nExp2)))
tc06 = TestCase (assertBool "Not exp1 == Not exp4 must be True" (nExp1 == nExp4))
tc07 = TestCase (assertBool "exp1 == Not exp4 must be False" (not(exp1 == nExp4)))
tc08 = TestCase (assertBool "(exp1 and exp2 == exp2 and exp1) must be True" (andExp1Exp2 == andExp2Exp1))
tc09 = TestCase (assertBool "(exp1 and exp2 == exp1 and exp3) must be False" (not (andExp1Exp2 == andExp1Exp3)))
tc10 = TestCase (assertBool "((not exp1) and exp2 == exp2 and (not exp1)) must be True" (andNExp1Exp2 == andExp2NExp1))
tc11 = TestCase (assertBool "(exp1 == exp1 and exp2 must be False" (not(exp1 == andExp1Exp2)))


tests = TestList [TestLabel "TC01" tc01, 
				  TestLabel "TC02" tc02, 
				  TestLabel "TC03" tc03,
				  TestLabel "TC04" tc04,
				  TestLabel "TC05" tc05,
				  TestLabel "TC06" tc06, 
				  TestLabel "TC07" tc07,
				  TestLabel "TC08" tc08]
