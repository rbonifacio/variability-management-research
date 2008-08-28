module UseCaseSampleEShop where


import UseCaseModel

-- **********************************************************
-- Some instances of UseCase model are created here.
-- **********************************************************

--ucmEshop = UCM "eShop use cases" [ucIddle,ucBuyProduct]
ucm01 = UCM "eShop use cases" [ucIddle, ucBuyProduct] []

-- ucBuyProduct = UseCase "UC01" "Buy products use case" "This use case allows the user to buy producuts" [scBuyProductBasic, scBuyProductCommon] 
ucBuyProduct = UseCase "UC01" "Buy products use case" "This use case allows the user to buy producuts" [scBuyProductCommon] 

aucBasicFlow = AspectualUseCase "A01" "Buy product - basic flow" [(BeforeAdvice [(IdRef "3M")] scBuyProductBasic)]
aucShoppingCart = AspectualUseCase "A02" "Buy products with shopping cart" [(BeforeAdvice [(IdRef "3M")] scShoppingCart)]
aucUpdatePreferences = AspectualUseCase "A03" "Update user preferences" [(AfterAdvice [(AnnotationRef "UpdatePreferences")] scUpdatePreferences)]

scBuyProductCommon = Scenario "1" "Common" [IdRef "2M", IdRef "V2"] [step3M, step4M, step5M] [IdRef "end"]
scBuyProductBasic = Scenario "2" "Main flow" [IdRef "start"] [step1M, step2M] [IdRef "3M"]
scShoppingCart = Scenario "3" "Buy product with shopping cart" [IdRef "start"] [stepV1, stepV2] [IdRef "3M"]
scUpdatePreferences = Scenario "4" "Register the user preferences" [AnnotationRef "UpdatePreferences"] [stepR1] [IdRef "end"]

step1M = Step "1M" scBuyProductBasic "Select the buy product option" "-" "Present the selected product. The user can change the quantity of item that he wants to buy. Calculate and show the ammount to be paid" []
step2M = Step "2M" scBuyProductBasic "Select the confirm option" "-" "Request payment information" []
step3M = Step "3M" scBuyProductCommon "Fill in the requested information and select the proceed option" "-" "Request the shipping method and address" []
step4M = Step "4M" scBuyProductCommon "Select the <ShipMethod>, fill in the destination address and proceed" "-" "Calculate the shipping costs" []
step5M = Step "5M" scBuyProductCommon "Confirm the purchase" "-" "Execute the order and send a request to the Delivery System to dispatch the products" ["UpdatePreferences"]

stepV1 = Step "V1" scShoppingCart "Select the checkout option" "-" "Present the items in the shopping cart and the amount to be paid. The user can remove items from shopping cart." []
stepV2 = Step "V2" scShoppingCart "Select the confirm option" "-" "Request bonus and payment information." []

stepR1 = Step "R1" scUpdatePreferences "-" "-" "Update the preferences based on the search results or purchased items." []