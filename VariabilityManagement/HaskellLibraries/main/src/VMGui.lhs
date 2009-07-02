\section{Graphical User Interface}

\begin{code}
module Main where

import Maybe

import List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import qualified Data.Tree as Tree

import Text.XML.HXT.Arrow
import System.Environment

-- import FeatureExpressionParser

-- import UseCaseModel.Parsers.XML.XmlUseCaseParser
import UseCaseModel.Parsers.XML.XmlUseCaseParser (parseUseCaseModel)
import UseCaseModel.Types

import FeatureModel.Parsers.GenericParser 

import FeatureModel.Types

import Transformations.Parsers.XML.XmlConfigurationParser
import ConfigurationKnowledge.Interpreter
import ConfigurationKnowledge.Types

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

  -- the feature model window
  fmWindow <- xmlGetWidget xml castToWindow "fmWindow" 
  
  -- list used for rendering configurations
  ckList <- xmlGetWidget xml castToTreeView "ckList"
  ckStore <- createCKStore
  New.treeViewSetModel ckList ckStore
  setupCKView ckList ckStore
 
  -- list for rendering errors
  errorList  <- xmlGetWidget xml castToTreeView "errorList"
  errorStore <- createErrorStore
  New.treeViewSetModel errorList errorStore
  setupErrorView errorList errorStore
  
  -- tree for rendering fm
  featureTree <- xmlGetWidget xml castToTreeView "featureTree" 
  featureStore <- createFeatureStore 
  New.treeViewSetModel featureTree featureStore
  setupFeatureView featureTree featureStore
  
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
  dfmtb <- xmlGetWidget xml castToToolButton "dfmtb"

  -- the following lines define actions related to the above buttons.

  -- click on start the weaving process tool button
  swptb `onToolButtonClicked` do 
       u  <- fileChooserGetFilename ucmFileChooser
       f  <- fileChooserGetFilename fmFileChooser
       p  <- fileChooserGetFilename pcFileChooser
       c  <- fileChooserGetFilename ckFileChooser
       executeBuildingProcess (fromJust f, fromJust p, fromJust u, fromJust c) 
  
  -- click on file checker tool button
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

  -- click on display feature model tool button
  dfmtb `onToolButtonClicked` do
       f <- fileChooserGetFilename fmFileChooser
       fm <-  parseFeatureModel' (fromJust f)
       let t = feature2TreeNode (fmTree fm) --(generateFmStore fm)
       New.treeStoreClear featureStore 
       New.treeStoreInsertTree featureStore [] 0 t
       widgetShowAll fmWindow

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

supportedFmTypes = [ ("xml", FMPlugin), (".m" , FMIde) ]

parseFeatureModel' fmfile = 
 let p = [snd t | t <- supportedFmTypes , (fst t) `isSuffixOf` fmfile]
 in case p of 
  [x] -> (parseFeatureModel fmfile x)
  otherwise -> error "Error identifying the feature model type"




{-------------------------------------------------------  
 Starts the building process.
 First, it retrieves the input models from the selected 
 files. Then, it executes the building process. 
--------------------------------------------------------}   
executeBuildingProcess :: (String, String, String, String) -> IO ()
executeBuildingProcess (fmFile, icFile, ucmFile, ckFile)= 
    do 
       fm   <- parseFeatureModel' fmFile 
       icTree   <- parseInstanceConfiguration icFile 
       splModel <- parseUseCaseModel ucmFile 
       ckModel  <- parseConfigurationKnowledge ckFile
           
       -- running the build process
       let fc = FeatureConfiguration icTree
       let result = build fm fc ckModel splModel 
       print $ iucm result


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

-- 
-- 
-- 
setupFeatureView view model = do {
  New.treeViewSetHeadersVisible view True;
      
  renderer1 <- New.cellRendererTextNew;
  col1 <- New.treeViewColumnNew;
  New.treeViewColumnPackStart col1 renderer1 True;
  New.cellLayoutSetAttributes col1 renderer1 model $ \row -> [ New.cellText := (feature2cell row) ];
  New.treeViewColumnSetTitle col1 "Name";
  New.treeViewAppendColumn view col1;

  
}
 
-- initialize error and ck stores
createErrorStore = New.listStoreNew []
createCKStore = New.listStoreNew []
createFeatureStore = New.treeStoreNew []

generateFmStore fm = (New.treeStoreNew [feature2TreeNode (fmTree fm)])

feature2TreeNode :: FeatureTree -> Tree.Tree Feature
feature2TreeNode (Leaf f) = Tree.Node { Tree.rootLabel = f, Tree.subForest = [] }
feature2TreeNode (Root f cs) = Tree.Node {Tree.rootLabel = f, Tree.subForest = (map feature2TreeNode cs) }

feature2cell f = 
 let 
  fn = if (fType f == Optional) 
        then "[" ++ (fName f) ++ "]" 
        else (fName f)  
 in 
  case (groupType f) of 
   AlternativeFeature -> fn ++ " g <1-1>"  
   OrFeature -> fn ++ " g <1-*>" 
   otherwise -> fn


\end{code}

  