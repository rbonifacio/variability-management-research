\begin{code}

module EShopUseCaseModel where

import UseCaseModel.Types
import BasicTypes


p1, p2, p3 :: Step

p1 = Step "P1"  
          "Fill in the requested information and select the proceed option"
          "-" 
          "Request the shipping method and address" 
          []
p2 = Step "P2" 
          "Select one of the vailable shipping methods {SM} , fill in..."
          "-"
          "Calculate the shipping costs" 
          []
p3 = Step "P3" 
          "Confirm the purchase" 
          "-" 
          "Execute the order and send a request to the..." 
          ["CustomerPreferences"]

sc01 :: Scenario
sc01 = Scenario "SC01" "Proceed to purchase" [] [p1,p2,p3] []

s1, s3 :: Step

s1 = Step "S1"
           "..."
           "..."
           "..."
           []
s3 = Step "S3" 
          "Inform the search criteria"
          "-"
          "Retrieve the products that satisfy the search criteria..."
          ["CustomerPreferences"]

sc02 :: Scenario
sc02 =  Scenario "SC02" "Search for products" [] [s1,s3] []
  
b1, b2 :: Step

b1 = Step "B1"  
          "Select the buy product option."
          "-" 
          "Present the selected product. The user can change the quantity of item..."
          []
b2 = Step "B1"  
          "Select the confirm option."
          "-" 
          "Request payment information"
          []

adv01 :: Advice
adv01 = BeforeAdvice [(IdRef "P1")] [b1,b2]

c1, c2 :: Step

c1 = Step "C1"  
          "Select the checkout option"
          "-" 
          "Present the items in the shopping cart and the amount ..."
          []
c2 = Step "C1"  
          "Select the confirm option."
          "-" 
          "Request bonus and payment information"
          []

adv02 :: Advice
adv02 = BeforeAdvice [(IdRef "P1")] [c1, c2]

r1 :: Step
r1 = Step "R1"
          "-"
          "-"
          "Update the preferences based on..."
          []

adv03 :: Advice
adv03 = AfterAdvice [(AnnotationRef "CustomerPreferences")] [r1]

uc01 :: UseCase
uc01 = UseCase "UC01" "Buy goods" "..." [sc01]

uc02 :: UseCase
uc02 = UseCase "UC02" "Search products" "..." [sc02]

asp01 :: AspectualUseCase
asp01 = AspectualUseCase "ADV01" "Buy single products" [adv01]

asp02 :: AspectualUseCase
asp02 = AspectualUseCase "ADV02" "Buy products with cart" [adv02]

asp03 :: AspectualUseCase
asp03 = AspectualUseCase "ADV03" "Register user preferences" [adv03]  

eShopUCM :: UseCaseModel

eShopUCM = UCM "eShop use case model" [uc01,uc02] [asp01,asp02,asp03]

\end{code}