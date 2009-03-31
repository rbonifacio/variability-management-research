\begin{code}

module EShopUseCaseModel where

import UseCaseModel.Types
import BasicTypes


p1, p2, p3 :: Step

p1 = Step "P1" sc01 
          "Fill in the requested information and select the proceed option"
          "-" 
          "Request the shipping method and address" 
          []
p2 = Step "P2" sc01
          "Select one of the vailable shipping methods {SM} , fill in..."
          "-"
          "Calculate the shipping costs" 
          []

p3 = Step "P3" sc01
          "Confirm the purchase" 
          "-" 
          "Execute the order and send a request to the..." 
          []

sc01 :: Scenario

sc01 = Scenario "SC01" "Proceed to purchase" [] [p1,p2,p3] []

uc01 :: UseCase

uc01 = UseCase "UC01" "Buy goods" "..." [sc01]

eShopUCM :: UseCaseModel

eShopUCM = UCM "eShop use case model" [uc01] []

\end{code}