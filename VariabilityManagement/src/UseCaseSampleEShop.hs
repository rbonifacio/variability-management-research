module UseCaseSampleEShop where

import TraceModel
import UseCaseModel
import FeatureModel
import Environment
import FeatureSample

-- **********************************************************
-- Some instances of UseCase model are created here.
-- **********************************************************

-- Basic message use case

ucBuyProduct = UseCase "UC01" "Buy products use case" "This use case allows the user to buy producuts" [] --, scenario3, scenario4]  
-- Main Scenario: Create and send a message

scBuyProductBasic = Scenario "1" "Main flow" [IdRef "start"] [step1M, step2M, step3M, step4M, step5M] [IdRef "end"]

step1M = Step "1M" scBuyProductBasic "Select the buy product option" "-" "Present the selected product. The user can change the quantity of item that he wants to buy. Calculate and show the ammount to be paid" []
step2M = Step "2M" scBuyProductBasic "Select the confirm option" "-" "Request payment information" []
step3M = Step "3M" scBuyProductBasic "Fill in the requested information and selecte the proceed option" "-" "Request the shipping method and address" []
step4M = Step "4M" scBuyProductBasic "Select the <ShipMethod>, fill in the destination address and proceed" "-" "Calculate the shipping costs" []
step5M = Step "5M" scBuyProductBasic "Confirm the purchase" "-" "Execute the order and send a request to the Delivery System to dispatch the products" []

