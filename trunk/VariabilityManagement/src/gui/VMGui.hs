module Main where

import Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import Text.XML.HXT.Arrow
import System.Environment

import FeatureExpressionParser

import XmlUseCaseParser
import XmlFeatureModel
import XmlFeatureParser

data UseCaseDocument = UseCaseDocument { path :: String }
data ConfigurationData = ConfigurationData { expressionData :: String , transformationData :: String }
 
main :: IO ()
main = do
  initGUI
  
  -- sets the glade file reference.
  -- with this reference, we can access the window widgets
  Just xml <- xmlNew "vm.glade"  
  
  -- the main window of our application
  -- when the main window is closed, the application finishes.
  window   <- xmlGetWidget xml castToWindow "mainWindow"
  onDestroy window mainQuit
  
  -- the configuration knowledge window
  -- this window is used for creating new configuration models; and
  -- for previewing existing ones.
  ckWindow   <- xmlGetWidget xml castToWindow "ckWindow";

  -- list used for rendering which use case documents 
  -- have been added into the build process. 
  -- this list is presented in the main window.
  useCaseList <- xmlGetWidget xml castToTreeView "useCasesList"
  useCaseStore <- createUseCaseStore
  New.treeViewSetModel useCaseList useCaseStore
  setupUseCaseView useCaseList useCaseStore
  
  -- list used for rendering configurations, which means 
  -- each entry of the configuration knowledge.
  -- this list (or tree view with a table format) is presented in the ck window
  ckList <- xmlGetWidget xml castToTreeView "ckList"
  ckStore <- createCKStore
  New.treeViewSetModel ckList ckStore
  setupCKView ckList ckStore
  
  -- the entry used for handling the selected feature model.
  featureModelEntry <- xmlGetWidget xml castToEntry "featureModelEntry"
  
  -- the entry used for handling feature expressions.
  -- note: this entry is presented in the ck window
  featureExpressionEntry <- xmlGetWidget xml castToEntry "featureExpressionEntry"
  
  -- the entry used for handling transformations
  -- note: this entry is presented in the ck window
  transformationListEntry <- xmlGetWidget xml castToEntry "transformationListEntry"
  
  -- the add use case button.
  -- this button is used for allowing the user to select 
  -- a new use case document as being part of the build process. 
  addUseCaseButton <- xmlGetWidget xml castToButton "addUseCaseButton"
  addUseCaseButton `onClicked` openSelectFileDialog useCaseStore window 
  
  -- the select feture model button
  -- this button is used for selecting the feature model 
  -- used in the build process.
  selectFMButton <- xmlGetWidget xml castToButton "selectFeatureModelButton"
  selectFMButton `onClicked` openSelectFMDialog featureModelEntry window
  
  -- the new configuration knowledge button
  -- this button is used for allowing the user to create a new 
  -- configuration model.
  newCKButton <- xmlGetWidget xml castToButton "newCKButton"
  newCKButton `onClicked` openNewCKDialog ckWindow
  
  -- the cancel button
  -- this button is used for canceling the build process, 
  -- finishing the application.
  cancelButton <- xmlGetWidget xml castToButton "cancelButton"
  onClicked cancelButton $ do widgetDestroy window
  
  -- the check feature expression button
  -- this button is used for performing a spell checking 
  -- in the feature expression entry. 
  checkFeatureExpButton <- xmlGetWidget xml castToButton "checkFeatureExpButton" 
  onClicked checkFeatureExpButton $ 
   do expressionTxt  <- entryGetText featureExpressionEntry
      let parseResult = featureExpressionParser expressionTxt
      let message = case parseResult of 
      		ParseResult x -> "Feature expression " ++ (show x) ++ " is correct"
      		ParseError y -> "Error parsing feature expression... on" ++ y
      messageDialog <- messageDialogNew (Just ckWindow) [] MessageInfo ButtonsClose message	
      widgetShowAll messageDialog	
      response <- dialogRun messageDialog
      widgetHide messageDialog
      

  -- the add configuration button
  -- this button is used for creating a new entry in the configuration 
  -- model.    
  addCKButton <- xmlGetWidget xml castToButton "addCKButton"
  onClicked addCKButton $ 
   do	expressionTxt  <- entryGetText featureExpressionEntry
      	transformationTxt <- entryGetText transformationListEntry
      	case (checkConfiguration expressionTxt transformationTxt) of
      		True -> do 	{
      						New.listStorePrepend ckStore  (ConfigurationData expressionTxt transformationTxt)
      					}	
      		False -> do {
      					 	dialog <- messageDialogNew (Just ckWindow) [] MessageInfo ButtonsClose "Both feature expression and transformations must be valid";
      					 	widgetShowAll dialog;
      					 	response <- dialogRun dialog;
      					  	widgetHide dialog;
  						}
  					 
  buttonQuitCK <- xmlGetWidget xml castToButton "buttonQuitCK"
  onClicked buttonQuitCK $ do widgetHide ckWindow
  
  executeButton <- xmlGetWidget xml castToButton "executeButton"
  onClicked executeButton $ 
   do fm <- entryGetText featureModelEntry
      [x] <- runX ( xunpickleDocument xpFeature [ (a_validate,v_0)
					, (a_trace, v_1)
					, (a_remove_whitespace,v_1)
					, (a_preserve_comment, v_0)
					] fm )
      print (xmlFeature2Feature x)	  	  				
  widgetShowAll window   
  mainGUI

-- Check if the new configuration is valid
-- In order to do that, both feature expression and transformation 
-- list must be well written.
checkConfiguration :: String -> String -> Bool
checkConfiguration e t = 
 case featureExpressionParser e of
 	ParseResult x -> True
 	ParseError y -> False 

--
-- Action related to addUseCaseButton
-- Opens a fileChooserDialog and defines the respective responses.
-- If the user selects a file, this file is added to the use case document store.  
--
openSelectFileDialog :: New.ListStore (UseCaseDocument) -> Window -> IO ()
openSelectFileDialog useCaseStore parentWindow = do
  dialog <- fileChooserDialogNew
              (Just $ "Select a XML use case document")  --dialog title
              (Just parentWindow)                    --the parent window
	      FileChooserActionOpen              		 --the kind of dialog we want
	      [("gtk-cancel"                                --The buttons to display
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
                         let ucDoc = UseCaseDocument { path = fileName } 
                         New.listStorePrepend useCaseStore ucDoc
    ResponseCancel -> putStrLn "dialog canceled"
    ResponseDeleteEvent -> putStrLn "dialog closed"
  widgetHide dialog



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
  

  