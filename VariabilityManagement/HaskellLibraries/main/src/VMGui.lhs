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

data ConfigurationData = ConfigurationData { expressionData :: String , transformationData :: String } deriving Show
data ErrorData = ErrorData { inputModel :: String, errorDesc :: String }
 
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

  errorList  <- xmlGetWidget xml castToTreeView "errorList"
  errorStore <- createErrorStore
  New.treeViewSetModel errorList errorStore
  setupErrorView errorList errorStore
  
  -- different entries for feature models, feature configurations 
  -- and configuration knowledge.
 
  ucmFileChooser <- xmlGetWidget xml castToFileChooserButton "ucmFileChooser"
  fmFileChooser  <- xmlGetWidget xml castToFileChooserButton "fmFileChooser" 
  pcFileChooser  <- xmlGetWidget xml castToFileChooserButton "pcFileChooser" 
  ckFileChooser  <- xmlGetWidget xml castToFileChooserButton "ckFileChooser"
   
  -- tool buttons for capturing user actions.
  -- name convension: 
  -- a) fctb  - button for checking input files
  -- b) sattb - button for checking fm satisfiability
  -- c) fbstb - button for finding fm bad smells
  -- d) swptb - button for start the weaving process   

  fctb  <- xmlGetWidget xml castToToolButton "cftb"
  sattb <- xmlGetWidget xml castToToolButton "sattb"
  fbstb <- xmlGetWidget xml castToToolButton "fbstb"
  swptb <- xmlGetWidget xml castToToolButton "swptb"

  -- the following lines define actions related to the above buttons.
  swptb `onToolButtonClicked` do 
       u  <- fileChooserGetFilename ucmFileChooser
       f  <- fileChooserGetFilename fmFileChooser
       p  <- fileChooserGetFilename pcFileChooser
       c  <- fileChooserGetFilename ckFileChooser
       executeBuildingProcess (fromJust f, fromJust p, fromJust u, fromJust c) 
  
  fctb `onToolButtonClicked` do 
       u <- fileChooserGetFilename ucmFileChooser
       f <- fileChooserGetFilename fmFileChooser
       p <- fileChooserGetFilename pcFileChooser
       c <- fileChooserGetFilename ckFileChooser 
       New.listStoreClear errorStore 
       executeFileChecker u "schema_aspectual-use_cases-user_view.rng" "Use case model" errorStore
       executeFileChecker f "schema_feature-model.rng" "Feature model" errorStore
       executeFileChecker p "schema_feature-configuration.rng" "Instance model" errorStore
       executeFileChecker c "schema-configuration-knowledge.rng" "Configuration knowledge" errorStore

--  cancelButton `onClicked` do widgetDestroy window
  
  
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

--
-- Check if a xml input file (f) adheres to the definitions 
-- of the schema (s). The store is updated with the list of 
-- errors. 
--
executeFileChecker Nothing s m store = updateErrorStore store [(ErrorData m "Model not loaded.")]
executeFileChecker (Just f) s m store = do { 
  errs <- runX ( errorMsgCollect 
                  >>> 
                  readDocument [(a_validate, v_0)
                               ,(a_relax_schema, s)
                               ,(a_issue_errors, "0")                              
                               ] f
                  >>>
                  getErrorMessages
                ) ;
  updateErrorStore store [ErrorData m (show e) | e <- errs]
 } 

-- update the error store, with a list of errors to 
-- be appendend. 
updateErrorStore s [] = return ()
updateErrorStore s (e:errs) = 
 do { 
   New.listStorePrepend s e; 
   updateErrorStore s errs
 }

--
-- an auxiliarly function to show simple dialogs
-- args: 
--  a) the parent window
--  b) the dialog type
--  c) the dialog message
showDialog w t  m = do
  messageDialog <- messageDialogNew (Just w) [] t ButtonsClose m	
  widgetShowAll messageDialog	
  response <- dialogRun messageDialog
  widgetHide messageDialog

-- 
-- Show the dialog for specifying configuration 
-- models. 
--
openNewCKDialog :: Window -> IO () 
openNewCKDialog ckWindow = 
 do {
	 widgetShowAll ckWindow;
 }
 
--
-- initilize the error tree list view. 
-- it mainly defines the columns rendered 
-- in the list viewer and relates such a columns
-- to the error model. 
-- 
setupErrorView view model = 
 do {
  New.treeViewSetHeadersVisible view True;

  renderer1 <- New.cellRendererTextNew;
  col1 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col1 renderer1 True;
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := inputModel row ];
  New.treeViewColumnSetTitle col1 "Input model";
  New.treeViewAppendColumn view col1;

  renderer2 <- New.cellRendererTextNew;
  col2 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col2 renderer2 True;
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := errorDesc row ];
  New.treeViewColumnSetTitle col2 "Description";
  New.treeViewAppendColumn view col2;
}

--
-- initialize the ck list view.
-- similarly to the setpErrorView.
-- 
setupCKView view model = do { 
  New.treeViewSetHeadersVisible view True;

  renderer1 <- New.cellRendererTextNew;
  col1 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col1 renderer1 True;
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := expressionData row ];
  New.treeViewColumnSetTitle col1 "Feature expression";
  New.treeViewAppendColumn view col1;
 
  renderer2 <- New.cellRendererTextNew;
  col2 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col2 renderer2 True;
  New.cellLayoutSetAttributes col2 renderer2 model $ \row -> [ New.cellText := show (transformationData row)];
  New.treeViewColumnSetTitle col2 "Transformations";
  New.treeViewAppendColumn view col2;
}
 
-- initialize error and ck stores
createErrorStore = New.listStoreNew [ ]
createCKStore = New.listStoreNew [ ]

\end{code}

  