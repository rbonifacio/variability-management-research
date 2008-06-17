module UseCaseSample where

import TraceModel
import UseCaseModel
import FeatureModel
import Environment
import FeatureSample

-- **********************************************************
-- Some instances of UseCase model are created here.
-- **********************************************************

-- Basic message use case

ucMessageBasic = UseCase "UC01" "Basic message use case" "Basic scenarios for messaging" [scenario1, scenario2] --, scenario3, scenario4]  
-- Main Scenario: Create and send a message
scenario1 = Scenario "1" "Main flow" [IdRef "start"] [step11, step12, step13, step14, step15, step16, step17, step18] [IdRef "end"]

step11 = Step "1M" scenario1 "Start the Message Center application" "-" "Message Center application is started" []
step12 = Step "2M" scenario1 "Select the Create Message option" "-" "The Create Message menu is displayed" []
step13 = Step "3M" scenario1 "Select <MessageType> from the Create Message Menu" "-" "The Create a New <MessageType> form is displayed" []
step14 = Step "4M" scenario1 "Fill the message body" "-" "The message body is filled" []
step15 = Step "5M" scenario1 "Select the Send Message option" "The message body is not empty" "The Recipient form is displayed" []
step16 = Step "6M" scenario1 "Fill the recipient field" "-" "The recipient field is filled" []
step17 = Step "7M" scenario1 "Select the Confirm Option" "The message box does not have <MaxMsg> messages" "The message is sent to recipient and saved in Sent Items folder. The Sent Message transient is displayed" []
step18 = Step "8M" scenario1 "Wait for the transient" "-" "Return to the Message Center application" []

-- Alternative Flow: Save message to draft folder
scenario2 = Scenario "2" "Save message to draft folder" [IdRef "4M", IdRef "5M"] [stepA11, stepA12] [IdRef "end"]

stepA11 = Step "A11" scenario2 "Select the Save Message option" "-" "The message is saved in Draft Folder. The Saved Message transient is displayed" ["SaveMessage"]
stepA12 = Step "A12" scenario2 "Wait for the transient" "-" "Return to the Message Center application" []

-- Alternative Flow: Save message to draft folder. The draft folder is full
scenario3 = Scenario "3" "Draft folder is full" [IdRef "A11"] [stepA21] [IdRef "A12"] 

stepA21 = Step "A21" scenario3 "Select the SaveMessage option" "The draft folder is full" "The Draft Folder is Full transient is displayed" []

-- Alternative Flow: Atach a multimedia content in a MMS composition
scenario4 = Scenario "4" "Atach a multimedia content" [IdRef "4M", IdRef "5M"][stepA31, stepA32, stepA33][IdRef "4M"] 

stepA31 = Step "A31" scenario4 "Select include a content type" "-" "The list of all content type resources is displayed" []
stepA32 = Step "A32" scenario4 "Browse to the desired content" "-" "The desired content is displayed" []
stepA33 = Step "A33" scenario4 "Select the confirm option" "-" "An attachment entry to the selected resource is created" []

---- **********************************
---- Use case: multiple folders 
---- **********************************
ucMfolder = UseCase "UC02" "Message folder management UC" "This use case enables the message multiple folder management" [scenario5, scenario6, scenario7, scenario8]

-- Main scenario: Create a new message folder
scenario5 = Scenario "5" "Create a new message folder" [IdRef "start"][step21, step22, step23, step24, step25][IdRef "end"]

step21 = Step "2.1M" scenario5 "Start the Message Center application" "-" "Message Center application is started" []
step22 = Step "2.2M" scenario5 "Select the Create a Message Folder option" "-" "The Create Message Folder form is displayed" []
step23 = Step "2.3M" scenario5 "Fill the required fields" "-" "The required fields are filled" []
step24 = Step "2.4M" scenario5 "Select the Confirm Option" "-" "The message folder is created. The Created Message Folder transient is displayed" []
step25 = Step "2.5M" scenario5 "Wait for the transient" "-" "Return to the Message Center application" []

-- Alternative scenario: Rename an existing message folder
scenario6 = Scenario "6" "Rename an existing message folder" [IdRef "2.2M"][stepA41, stepA42, stepA43, stepA44][IdRef "end"]

stepA41 = Step "A41" scenario6 "Select an existing message folder" "-" "The selected folder is highlighted" []
stepA42 = Step "A42" scenario6 "Select the Rename Folder option" "-" "The Rename Message Folder form is displayed" []
stepA43 = Step "A43" scenario6 "Fill the new name of the message folder" "-" "The message folder is renamed. The Folder was Renamed transient is displayed" []
stepA44 = Step "A44" scenario6 "Wait for the transient" "-" "Return to the Message Center application" []

-- Alternative scenario: Delete an existing message folder
scenario7 = Scenario "7" "Delete an existing message folder" [IdRef "A42"][stepA51, stepA52, stepA53][IdRef "end"]

stepA51 = Step "A51" scenario7 "Select the Delete Folder option" "-" "The All Message in the Selected Folder alert is displayed" []
stepA52 = Step "A52" scenario7 "Select the Confirm Option" "-" "All messages in the selected folder are deleted. The message folder is deleted. The Folder was Excluded transient is displayed" []
stepA53 = Step "A53" scenario7 "Wait for the transient" "-" "Return to the Message Center application" []

-- Alternative scenario: Create and save a message with multiple folders
-- This scenario exludes the scenario2 - Save message do draft folder and 

scenario8 = Scenario "8" "Save a message in the selected folder" [IdRef "4M", IdRef "5M"][stepA61,stepA62,stepA63,stepA64][IdRef "end"]

stepA61 = Step "A61" scenario8 "Select the save message option" "-" "The list of message folders is displayed. The draft folder is highlighted" []
stepA62 = Step "A62" scenario8 "Browse to the desired message folder" "-" "The selected message folder is highlighted" []
stepA63 = Step "A63" scenario8 "Select the Confirm option" "-" "The message is saved in the selected folder. The Saved Message transient is displayed" ["SaveMessage"]
stepA64 = Step "A64" scenario8 "Wait for the transient" "-" "Return to the Message Center application" []

---- **********************************
---- Use case: Compressed message 
---- **********************************
ucMessageCompression = UseCase "UC03" "Message compression" "This use case compress message before save" [scenario9]

scenario9 = Scenario "9" "Compress the message before save" [AnnotationRef "SaveMessage"][step31, step32][IdRef "end"]

step31 = Step "3.1M" scenario9 "Select the confirm option" "-" "The message is compressed and saved. The Saved Message transient is displayed" []
step32 = Step "3.2M" scenario9 "Wait for the transient" "-" "Return to the Message Center application" []

ucm = UCM "Use case application" [ucIddle, ucMessageBasic,ucMfolder, ucMessageCompression] 

-- 
-- The environment that relates the parameters with the feature model.
-- TODO: implement a function that creates an environment based on a feature
-- configuration.



