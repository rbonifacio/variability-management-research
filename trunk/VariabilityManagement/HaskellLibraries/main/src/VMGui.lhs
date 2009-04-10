\section{Graphical User Interface}

\begin{code}
module Main where

import Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import Text.XML.HXT.Arrow
import System.Environment

-- import FeatureExpressionParser

import UseCaseModel.Parsers.XML.XmlUseCaseParser
import UseCaseModel.Parsers.XML.XmlUseCaseModel
import UseCaseModel.Types

import FeatureModel.Parsers.FMPlugin.XmlFeatureModel
import FeatureModel.Parsers.FMPlugin.XmlFeatureParser
import FeatureModel.Types

import UseCaseModel.Parsers.XML.XmlConfigurationParser
import UseCaseModel.Parsers.XML.XmlConfigurationKnowledge
import ConfigurationKnowledge.Interpreter

data UseCaseDocument = UseCaseDocument { path :: String } deriving Show
data ConfigurationData = ConfigurationData { expressionData :: String , transformationData :: String } deriving Show
 
main :: IO ()
main = do
  initGUI
  
  -- sets the glade file reference.
  -- with this reference, we can access the window widgets
  Just xml <- xmlNew "vm.glade"  
  
  -- the main window of VM application
  window   <- xmlGetWidget xml castToWindow "mainWindow"
  onDestroy window mainQuit
  
  -- the configuration knowledge window
  ckWindow   <- xmlGetWidget xml castToWindow "ckWindow"
  
  -- list used for rendering configurations
  ckList <- xmlGetWidget xml castToTreeView "ckList"
  ckStore <- createCKStore
  New.treeViewSetModel ckList ckStore
  setupCKView ckList ckStore
  
  -- different entries for feature models, feature configurations 
  -- and configuration knowledge.
 
  ucmFileChooser <- xmlGetWidget xml castToFileChooserButton "ucmFileChooser"
  fmFileChooser  <- xmlGetWidget xml castToFileChooserButton "fmFileChooser" 
  pcFileChooser  <- xmlGetWidget xml castToFileChooserButton "pcFileChooser" 
  ckFileChooser  <- xmlGetWidget xml castToFileChooserButton "ckFileChooser"
   
  -- buttons for capturing user actions. 
  executeButton        <- xmlGetWidget xml castToButton "executeButton"
  ucmXmlCheckerButton  <- xmlGetWidget xml castToButton "ucmXMLCheckerButton"
-- selectFMButton       <- xmlGetWidget xml castToButton "selectFeatureModelButton"
-- selectInstanceButton <- xmlGetWidget xml castToButton "selectInstanceButton"
-- selectCKButton       <- xmlGetWidget xml castToButton "selectCKButton"
-- newCKButton          <- xmlGetWidget xml castToButton "newCKButton"
  cancelButton         <- xmlGetWidget xml castToButton "cancelButton"
 
  -- the following lines define actions related to the above buttons.
  executeButton `onClicked` do 
      u  <- fileChooserGetFilename ucmFileChooser
      f  <- fileChooserGetFilename fmFileChooser
      p  <- fileChooserGetFilename pcFileChooser
      c  <- fileChooserGetFilename ckFileChooser
      executeBuildingProcess (fromJust f, fromJust p, fromJust u, fromJust c) 
  
  ucmXmlCheckerButton `onClicked` do 
      u <- fileChooserGetFilename ucmFileChooser
      let e = executeUcmXmlChecker (u) 
      let (x,y) = case e of 
                   [] -> (MessageInfo, "No error found in use case model!" )
                   otherwise -> (MessageError, "Error: " ++ (show e))
      showDialog window x y  
   
  cancelButton `onClicked` do widgetDestroy window
  
  
  -- the check feature expression button
  -- this button is used for performing a spell checking 
  -- in the feature expression entry. 
 --  checkFeatureExpButton <- xmlGetWidget xml castToButton "checkFeatureExpButton" 
--   onClicked checkFeatureExpButton $ 
--    do expressionTxt  <- entryGetText featureExpressionEntry
--       let parseResult = featureExpressionParser expressionTxt
--       let message = case parseResult of 
--       		ParseExpressionResult x -> "Feature expression " ++ (show x) ++ " is correct"
--       		ParseError y -> "Error parsing feature expression... on " ++ y
--       messageDialog <- messageDialogNew (Just ckWindow) [] MessageInfo ButtonsClose message	
--       widgetShowAll messageDialog	
--       response <- dialogRun messageDialog
--       widgetHide messageDialog

  -- the check transformation button
  -- this button is used for performing a spell checking in the 
  -- list of transformations entry 
--   checkTransformationButton <- xmlGetWidget xml castToButton "buttonTransformationListCheck"
--   onClicked checkTransformationButton $ 
--    do transformationTxt <- entryGetText transformationListEntry
--       let parseResult = transformationParser transformationTxt
--       let message = case parseResult of
--                 ParseTransformationResult x -> "List of transformations " ++ transformationTxt ++ " is correct"
--                 ParseError y -> "Error parsing feature expression... on " ++ y
--       messageDialog <- messageDialogNew (Just ckWindow) [] MessageInfo ButtonsClose message
--       widgetShowAll messageDialog
--       response <- dialogRun messageDialog
--       widgetHide messageDialog
 
      

  -- the add configuration button
  -- this button is used for creating a new entry in the configuration 
  -- model.    
 --  addCKButton <- xmlGetWidget xml castToButton "addCKButton"
--   onClicked addCKButton $ 
--    do	expressionTxt  <- entryGetText featureExpressionEntry
--       	transformationTxt <- entryGetText transformationListEntry
--       	case (checkConfiguration expressionTxt transformationTxt) of
--       		True -> do {
--       			    New.listStorePrepend ckStore  (ConfigurationData expressionTxt transformationTxt)
--       			}	
--       		False -> do {
--       			     dialog <- messageDialogNew (Just ckWindow) 
--                                        [] 
--                                        MessageInfo ButtonsClose "Both feature expression and transformations must be valid";
      					 	
--                              widgetShowAll dialog;
--       			     response <- dialogRun dialog;
--       			     widgetHide dialog;
--   			 }
  					 
  buttonQuitCK <- xmlGetWidget xml castToButton "buttonQuitCK"
  onClicked buttonQuitCK $ do widgetHide ckWindow
  
  widgetShowAll window   
  mainGUI

{-------------------------------------------------------  
 Starts the building process.
 First, it retrieves the input models from the selected 
 files. Then, it executes the building process. 
--------------------------------------------------------}   
executeBuildingProcess :: (String, String, String, String) -> IO ()
executeBuildingProcess (fmfile, instancefile, ucmfile, ckfile)= 
    do 
       -- loading the feature model
       -- fmfile <- entryGetText featureModelEntry
       putStrLn "Reading feature model... "
       putStrLn "" 
       [f] <- runX ( xunpickleDocument xpFeature [ (a_validate,v_0)
 					, (a_trace, v_1)
 					, (a_remove_whitespace,v_1)
 					, (a_preserve_comment, v_0)
 					] fmfile )
       let ftree = xmlFeature2FeatureTree f
       putStrLn $ show ftree
       putStrLn "Done."
       putStrLn "================================="

       -- loading the instance model
       -- instancefile <- entryGetText instanceEntry
       putStrLn "Reading instance model... "
       putStrLn ""
       [i] <- runX ( xunpickleDocument xpFeatureConfiguration [ (a_validate,v_0)
 					, (a_trace, v_1)
 					, (a_remove_whitespace,v_1)
 					, (a_preserve_comment, v_0)
 					] instancefile )
       let itree = xml2FeatureConfiguration i
       putStrLn $ show itree
       putStrLn "Done. "
       putStrLn "==================================="

       -- loading the use case model
       -- ucms <- listStoreToList useCaseStore 
       -- let ucmfile = path (head ucms) -- TODO: we should load all documents, instead of just one
       putStrLn "Reading use case model..."  
       putStrLn ""
       [u] <- runX ( xunpickleDocument xpUseCaseModel [ (a_validate,v_0)
 					, (a_trace, v_1)
 					, (a_remove_whitespace,v_1)
 					, (a_preserve_comment, v_0)
 					] ucmfile )
       let splmodel = xmlUseCaseModel2UseCaseModel u
       putStrLn $ show splmodel
       putStrLn "Done."
       putStrLn "=====================================" 
      
       -- loading the configuration knowledge
       -- ckfile <- entryGetText configurationEntry
       print "\n Reading the configuration model... "
       [c] <- runX ( xunpickleDocument xpConfigurationKnowledge [ (a_validate,v_0)
 				      , (a_trace, v_1)
 				      , (a_remove_whitespace,v_1)
 				      , (a_preserve_comment, v_0)
 				      ] ckfile )
       let ck = xml2ConfigurationKnowledge c
       putStrLn "Done."
       putStrLn $ "Configuration items: " ++ show (length ck)
       putStrLn "===================================="
       
       -- running the build process
       let fm = FeatureModel ftree []
       let fc = FeatureConfiguration itree
       let r = build fm fc ck splmodel 
       print $ (snd r)


targetSchema :: String
targetSchema = "schema_aspectual-use_cases-user_view.rng" 

{------------------------------------------ 
 Check if the UCM xml file is correct. 
 Return a list of errors. 
-------------------------------------------}

executeUcmXmlChecker :: (Maybe FilePath) -> [String]
executeUcmXmlChecker Nothing = ["A ucm file must be selected."]
executeUcmXmlChecker (Just f)  = []
--  rc <- runX ( readDocument [(a_validate,v_0),
--                             (a_relax_schema, targerSchema)
--                            ] src
--               >>>
--               validateDocumentWithRelaxSchema [] targetSchema
--               >>>
--               writeDocument [] dst
--               >>> 
--               getErrStatus
--             )


showDialog w t  m = do
  messageDialog <- messageDialogNew (Just w) [] t ButtonsClose m	
  widgetShowAll messageDialog	
  response <- dialogRun messageDialog
  widgetHide messageDialog

-- checkUseCases :: New.ListStore (UseCaseDocument) -> IO ()
-- checkUseCases useCaseStore = do 
--   ucms <- listStoreToList useCaseStore 
--   let ucmfile = path (head ucms) -- TODO: we should load all documents, instead of just one
--   putStrLn "Reading use case model..."  
--   putStrLn ""
--   [u] <- runX ( xunpickleDocument xpUseCaseModel [ (a_validate,v_0)
--   				  , (a_trace, v_1)
-- 				  , (a_remove_whitespace,v_1)
-- 				  , (a_preserve_comment, v_0)
--                                   , (a_issue_errors, v_1)
-- 				  ] ucmfile )
--  print u  

 

-- Check if the new configuration is valid
-- In order to do that, both feature expression and transformation 
-- list must be well written.
checkConfiguration :: String -> String -> Bool
checkConfiguration e t = True 
 -- case featureExpressionParser e of
 	-- ParseResult x -> True
 	-- ParseError y -> False 

--
-- Action related to addUseCaseButton
-- Opens a fileChooserDialog and defines the respective responses.
-- If the user selects a file, this file is added to the use case document store.  
--
-- openSelectFileDialog :: New.ListStore (UseCaseDocument) -> Window -> IO ()
-- openSelectFileDialog useCaseStore parentWindow = do
--   dialog <- fileChooserDialogNew
--               (Just $ "Select a XML use case document")  --dialog title
--               (Just parentWindow)                        --the parent window
-- 	      FileChooserActionOpen                      --the kind of dialog we want
-- 	      [("gtk-cancel"                             --The buttons to display
-- 	       ,ResponseCancel)
-- 	       ,("gtk-open"                                  
-- 	       , ResponseAccept)]
--   ffilter <- fileFilterNew
--   fileFilterSetName ffilter "XML feature model document"
--   fileFilterAddPattern ffilter "*.xml"
--   fileChooserSetFilter dialog ffilter	       
--   widgetShow dialog
--   response <- dialogRun dialog
--   case response of
--     ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
--                          let ucDoc = UseCaseDocument { path = fileName } 
--                          New.listStorePrepend useCaseStore ucDoc
--     ResponseCancel -> putStrLn "dialog canceled"
--     ResponseDeleteEvent -> putStrLn "dialog closed"
--  widgetHide dialog



--
-- Action related to selectFetureModelButton
-- Opens a fileChooserDialog and defines the respective responses.
 --  

openSelectFMDialog :: Entry -> Window -> IO ()
openSelectFMDialog fmEntry parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Select a XML feature model")    --dialog title
              (Just parentWindow)                      --the parent window
	      FileChooserActionOpen              		   --the kind of dialog we want
	      [("gtk-cancel"                               --The buttons to display
	       ,ResponseCancel)
	       ,("gtk-open"                                  
	       , ResponseAccept)]
  ffilter <- fileFilterNew
  fileFilterSetName ffilter "XML feature model document"
  fileFilterAddPattern ffilter "*.xml"
  fileChooserSetFilter dialog ffilter	       
  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         entrySetText fmEntry fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openSelectInstanceDialog :: Entry -> Window -> IO ()
openSelectInstanceDialog instanceEntry parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Select a XML instance model")             --dialog title
              (Just parentWindow)                                --the parent window
	      FileChooserActionOpen              		 --the kind of dialog we want
	      [("gtk-cancel"                                     --The buttons to display
	       ,ResponseCancel)
	       ,("gtk-open"                                  
	       , ResponseAccept)]
  ffilter <- fileFilterNew
  fileFilterSetName ffilter "XML instance model"
  fileFilterAddPattern ffilter "*.xml"
  fileChooserSetFilter dialog ffilter	       
  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         entrySetText instanceEntry fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog


openSelectCKDialog :: Entry -> Window -> IO ()
openSelectCKDialog ckEntry parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Select a XML configuration knowledge")    --dialog title
              (Just parentWindow)                                --the parent window
	      FileChooserActionOpen              		 --the kind of dialog we want
	      [("gtk-cancel"                                     --The buttons to display
	       ,ResponseCancel)
	       ,("gtk-open"                                  
	       , ResponseAccept)]
  ffilter <- fileFilterNew
  fileFilterSetName ffilter "XML condifuration document"
  fileFilterAddPattern ffilter "*.xml"
  fileChooserSetFilter dialog ffilter	       
  widgetShow dialog
  response <- dialogRun dialog
  case response of 
    ResponseAccept -> do Just fileName <- fileChooserGetFilename dialog
                         entrySetText ckEntry fileName
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog

openNewCKDialog :: Window -> IO () 
openNewCKDialog ckWindow = 
 do {
	 widgetShowAll ckWindow;
 }
 
-- 
-- This function configures the UseCaseView, which means:
-- - Sets the use case view presentations
-- - Defines the use case view columns
-- - Relates each column to an attribute of UseCaseDocument data type
-- 
setupUseCaseView view model = do
  New.treeViewSetHeadersVisible view True

  renderer1 <- New.cellRendererTextNew
  col1 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col1 renderer1 True
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := path row ]
  New.treeViewColumnSetTitle col1 "Use case document"
  New.treeViewAppendColumn view col1

setupCKView view model = do 
  New.treeViewSetHeadersVisible view True

  renderer1 <- New.cellRendererTextNew
  col1 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col1 renderer1 True
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := expressionData row ]
  New.treeViewColumnSetTitle col1 "Feature expression"
  New.treeViewAppendColumn view col1
 
  renderer2 <- New.cellRendererTextNew
  col2 <- New.treeViewColumnNew
  New.treeViewColumnPackStart col2 renderer2 True
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := show (transformationData row)]
  New.treeViewColumnSetTitle col2 "Transformations"
  New.treeViewAppendColumn view col2
 
createUseCaseStore = New.listStoreNew [ ]
createCKStore = New.listStoreNew [ ]

-- createUseCaseStore = New.listStoreNew [UseCaseDocument { path = "foo.xml" }]


--  renderer2 <- New.cellRendererTextNew
--  col2 <- New.treeViewColumnNew
--  New.treeViewColumnPackStart col2 renderer2 True
--  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := show (number row) ]
--  New.treeViewColumnSetTitle col2 "Int column"
--  New.treeViewAppendColumn view col2

--  renderer3 <- New.cellRendererToggleNew
--  col3 <- New.treeViewColumnNew
--  New.treeViewColumnPackStart col3 renderer3 True
--  New.cellLayoutSetAttributes col3 renderer3 model $ \row -> [ New.cellToggleActive := marked row ]
--  New.treeViewColumnSetTitle col3 "Check box column"
--  New.treeViewAppendColumn view col3  
  

\end{code}

  