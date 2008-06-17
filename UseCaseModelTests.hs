
module UseCaseModelTests where

import UseCaseModel
import Test.HUnit

ucm = UCM "Use case application" [ucIddle, uc01] 
uc01 = UseCase "UC01" "Test" "Test" [scenario1, scenario2] 

scenario1 = Scenario "1" "Main flow" [IdRef "start"] [step11, step12, step13, step14] [IdRef "M2.1"]
scenario2 = Scenario "2" "Extension flow - LOOP" [IdRef "start"] [step21, step22] [IdRef "M1.4"]

step11 = Step "M1.1" scenario1 "UA-11" "-" "SR-11" []
step12 = Step "M1.2" scenario1 "UA-12" "-" "SR-12" []
step13 = Step "M1.3" scenario1 "UA-13" "-" "SR-13" []
step14 = Step "M1.4" scenario1 "UA-14" "-" "SR-14" []

step21 = Step "M2.1" scenario2 "UA-21" "-" "SR-21" []
step22 = Step "M2.2" scenario2 "UA-22" "-" "SR-22" []