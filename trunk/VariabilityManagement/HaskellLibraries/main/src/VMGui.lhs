\section{Graphical User Interface}

\begin{code}
module Main where

import qualified BasicTypes as Core

import Maybe


import List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import qualified Data.Tree as Tree

import Text.XML.HXT.Arrow
import System.Environment

import UseCaseModel.PrettyPrinter.Latex
import UseCaseModel.Parsers.XML.XmlUseCaseParser (parseUseCaseFile, checkUseCaseFile)
import UseCaseModel.Types

import FeatureModel.Parsers.GenericParser 

import FeatureModel.Types

import Transformations.Parsers.XML.XmlConfigurationParser
import ConfigurationKnowledge.Interpreter
import ConfigurationKnowledge.Types

import ComponentModel.Parsers.ParserComponentModel

data ConfigurationData = ConfigurationData { expressionData :: String , transformationData :: String } deriving Show
data ErrorData = ErrorData { inputModel :: String, errorDesc :: String }

data GUI = GUI {
      window :: Window, 
      ckWindow :: Window, 
      fmWindow :: Window, 
      ucmFChooser :: FileChooserButton,
      cmFChooser  :: FileChooserButton,  
      fmFChooser  :: FileChooserButton, 
      pcFChooser  :: FileChooserButton, 
      ckFChooser  :: FileChooserButton,
      outFChooser :: FileChooserButton, 
      ckList     :: TreeView,
      errList    :: TreeView,
      featureTree    :: TreeView,
      fcheckerTButton  :: ToolButton,
      satTButton       :: ToolButton,
      badSmellsTButton :: ToolButton,
      weavingTButton   :: ToolButton, 
      displayFmTButton :: ToolButton
}

main :: IO ()
main = do
  initGUI
  
  Just gladefile <- xmlNew "vm.glade"  
  
  gui <- loadGlade gladefile

  connectGui gui 


loadGlade f = 
 do
   [w, ckw, fmw] <- mapM (xmlGetWidget f castToWindow) ["mainWindow"
                                                       ,"ckWindow"
                                                       ,"fmWindow"
                                                       ]  
   
                 
  
   [ucmfc, cmfc, fmfc, pcfc, ckfc, outfc]  <- mapM (xmlGetWidget f castToFileChooserButton) ["ucmFileChooser"
                                                                                            ,"cmFileChooser"
                                                                                            ,"fmFileChooser"
                                                                                            ,"pcFileChooser"
                                                                                            ,"ckFileChooser"
                                                                                            ,"outputFileChooser"
                                                                                            ]
  
   [ckl, errl, ftree] <- mapM (xmlGetWidget f castToTreeView) ["ckList"
                                                              , "errorList"
                                                              , "featureTree"
                                                              ] 
   

   [fctb, sattb, fbstb, swptb, dfmtb] <- mapM (xmlGetWidget f castToToolButton) ["cftb"
                                                                                , "sattb"
                                                                                , "fbstb"
                                                                                , "swptb"
                                                                                , "dfmtb"
                                                                                ]
   return $ GUI {
                window      = w, 
                ckWindow    = ckw, 
                fmWindow    = fmw, 
                ucmFChooser = ucmfc,
                cmFChooser  = cmfc,
                fmFChooser  = fmfc,
                pcFChooser  = pcfc,
                ckFChooser  = ckfc,
                outFChooser = outfc,
                ckList      = ckl,
                errList     = errl,
                featureTree = ftree, 
                fcheckerTButton  = fctb,
                satTButton  = sattb,
                badSmellsTButton = fbstb,
                weavingTButton = swptb, 
                displayFmTButton = dfmtb
              }

connectGui gui = 
 do
  onDestroy (window gui) mainQuit
  onDestroy (fmWindow gui)$ widgetHide (fmWindow gui)
  
  ckStore <- createCKStore
  New.treeViewSetModel (ckList gui) ckStore
  setupCKView (ckList gui) ckStore
 
  errorStore <- createErrorStore
  New.treeViewSetModel (errList gui) errorStore
  setupErrorView (errList gui) errorStore
  
  featureStore <- createFeatureStore 
  New.treeViewSetModel (featureTree gui) featureStore
  setupFeatureView (featureTree gui) featureStore
  
  onToolButtonClicked (weavingTButton gui)   (weaveFiles gui)
  onToolButtonClicked (fcheckerTButton gui)  (checkFiles gui errorStore)
  onToolButtonClicked (displayFmTButton gui) (displayFeatureModel gui featureStore) 
  
  widgetShowAll (window gui)   
  mainGUI

{-------------------------------------------------------------------          
---------------------  GUI related events --------------------------        
--------------------------------------------------------------------}

-- 
-- starts the weaving process (or product derivation process) 
-- when the user clicks on the "generate products" button. 
--
weaveFiles gui = 
 do 
   selectedFiles <- mapM fileChooserGetFilename [ucmFChooser gui
                                                ,cmFChooser  gui
                                                ,fmFChooser  gui
                                                ,pcFChooser  gui
                                                ,ckFChooser  gui
                                                ,outFChooser gui
                                                ]
   case selectedFiles of 
     [Just u, Just c, Just f, Just p, Just ck, Just o] -> do executeBuildingProcess (u, c, f, p, ck, o)
     otherwise -> showDialog  (window gui) 
                              MessageError 
                              "Error on input files. Try checking these files before starting the weaving process."  

--
-- starts file checker process, updating 
-- the error list if any error is found. 
-- 
checkFiles gui store = 
 do
   u <- fileChooserGetFilename (ucmFChooser gui)
   f <- fileChooserGetFilename (fmFChooser  gui)
   p <- fileChooserGetFilename (pcFChooser gui)
   c <- fileChooserGetFilename (ckFChooser gui)
   New.listStoreClear store
   executeFileChecker u "schema_aspectual-use_cases-user_view.rng" "Use case model" store
   executeFileChecker f "schema_feature-model.rng" "Feature model" store
   executeFileChecker p "schema_feature-configuration.rng" "Instance model" store
   executeFileChecker c "schema-configuration-knowledge.rng" "Configuration knowledge" store

displayFeatureModel gui store = 
 do
   f <- fileChooserGetFilename (fmFChooser gui)
   case f of 
     (Just fName) -> 
         do 
           fm <-  parseFeatureModel' fName
           let t = feature2TreeNode (fmTree fm) 
           New.treeStoreClear store 
           New.treeStoreInsertTree store [] 0 t
           widgetShowAll (fmWindow gui)
     otherwise -> showDialog (window gui) 
                             MessageError 
                             "Please, select a feature model."

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
executeBuildingProcess :: (String, String, String, String, String, String) -> IO ()
executeBuildingProcess (ucmFile, cmFile, fmFile, icFile, ckFile, outFile) = 
    do 
       fm            <- parseFeatureModel' fmFile 
       cmParseResult <- parseComponentModel cmFile
       icTree        <- parseInstanceConfiguration icFile 
       ucParseResult <- parseUseCaseFile ucmFile "schema_aspectual-use_cases-user_view.rng" 
       ckModel       <- parseConfigurationKnowledge ckFile    
        
       case (ucParseResult, cmParseResult) of 
        (Core.Success ucm, Core.Success cm) -> do 
                        let fc  = FeatureConfiguration icTree
                        let spl = SPLModel fm ucm cm 
                        let result = build fm fc ckModel spl
                        print $ ucmToLatex (iucm result)
                        print $ components result
        
        (Core.Fail s, _) -> putStrLn s
        (_, Core.Fail s) -> putStrLn s
      	       
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

  