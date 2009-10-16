\section{Graphical User Interface}

\begin{code}
module Main where

import qualified BasicTypes as Core

import Maybe

import List
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New
import IO
import System
import qualified Data.Tree as Tree

import Text.XML.HXT.Arrow
import System.Environment

import RequirementModel.Types
import RequirementModel.Parsers.XML.XmlRequirementParser

import UseCaseModel.PrettyPrinter.Latex
import UseCaseModel.Parsers.XML.XmlUseCaseParser (parseUseCaseFile, checkUseCaseFile)
import UseCaseModel.Types

import FeatureModel.Types
import FeatureModel.Parsers.GenericParser 

import Transformations.Parsers.XML.XmlConfigurationParser
import ConfigurationKnowledge.Interpreter
import ConfigurationKnowledge.Types

import ComponentModel.Parsers.ParserComponentModel

data ConfigurationData = ConfigurationData { expressionData :: String , transformationData :: String } deriving Show
data ErrorData = ErrorData { inputModel :: String, errorDesc :: String }

fmSchema :: String 
fmSchema = "schema_feature-model.rng"

rmSchema :: String 
rmSchema = "schema_requirements.rng"

ucSchema :: String
ucSchema = "schema_aspectual-use_cases-user_view.rng" 

fcSchema :: String 
fcSchema = "schema_feature-configuration.rng"

ckSchema :: String
ckSchema = "schema-configuration-knowledge.rng"

copy :: String
copy = "cp " 

mkdir :: String 
mkdir = "mkdir -p " 

data GUI = GUI {
      window :: Window, 
      ckWindow :: Window, 
      fmWindow :: Window, 
      rmFChooser  :: FileChooserButton, 
      ucmFChooser :: FileChooserButton,
      cmFChooser  :: FileChooserButton,  
      fmFChooser  :: FileChooserButton, 
      pcFChooser  :: FileChooserButton, 
      ckFChooser  :: FileChooserButton,
      sdFChooser  :: FileChooserButton, 
      outFChooser :: FileChooserButton, 
      ckList     :: TreeView,
      errList    :: TreeView,
      featureTree    :: TreeView,
      fcheckerTButton  :: ToolButton,
      satTButton       :: ToolButton,
      badSmellsTButton :: ToolButton,
      weavingTButton   :: ToolButton, 
      displayFmTButton :: ToolButton,
      editCkTButton    :: ToolButton
}

-- 
-- The entry point of our applicaton. Basically, 
-- it retrieves the GUI definition file, instantiate 
-- a GUI model and connect it to user action events. 
-- 
main :: IO ()
main = do
  initGUI
  
  Just gladefile <- xmlNew "vm.glade"  
  
  gui <- loadGlade gladefile

  connectGui gui 

-- --------------------------------------------------------------------------
-- Load an instance of GUI the data type.
-- This design is based on the Real World Haskell book 
-- --------------------------------------------------------------------------
loadGlade f = 
 do
   -- retrieve the GUI windows
   [w, ckw, fmw] <- mapM (xmlGetWidget f castToWindow) ["mainWindow"
                                                       ,"ckWindow"
                                                       ,"fmWindow"
                                                       ]  
   
                 
   -- retrieves the file chooser elements.
   [rmfc, ucmfc, cmfc, fmfc, pcfc, ckfc, sdfc, outfc]  <- mapM (xmlGetWidget f castToFileChooserButton) ["rmFileChooser"
                                                                                                        ,"ucmFileChooser"
                                                                                                        ,"cmFileChooser"
                                                                                                        ,"fmFileChooser"
                                                                                                        ,"pcFileChooser"
                                                                                                        ,"ckFileChooser"
                                                                                                        ,"sdFileChooser"  
                                                                                                        ,"outputFileChooser"
                                                                                                        ]
   -- retrieves the tree view and list elements
   [ckl, errl, ftree] <- mapM (xmlGetWidget f castToTreeView) ["ckList"
                                                              , "errorList"
                                                              , "featureTree"
                                                              ] 
   -- retrieves the tool buttons
   [fctb, sattb, fbstb, swptb, dfmtb, eck] <- mapM (xmlGetWidget f castToToolButton) ["cftb"
                                                                                     , "sattb"
                                                                                     , "fbstb"
                                                                                     , "swptb"
                                                                                     , "dfmtb"
                                                                                     , "eck"
                                                                                     ]
   -- returns the GUI instance                                                                             
   return $ GUI {
                window      = w, 
                ckWindow    = ckw, 
                fmWindow    = fmw, 
                rmFChooser  = rmfc, 
                ucmFChooser = ucmfc,
                cmFChooser  = cmfc,
                fmFChooser  = fmfc,
                pcFChooser  = pcfc,
                ckFChooser  = ckfc,
                sdFChooser  = sdfc,
                outFChooser = outfc,
                ckList      = ckl,
                errList     = errl,
                featureTree = ftree, 
                fcheckerTButton  = fctb,
                satTButton  = sattb,
                badSmellsTButton = fbstb,
                weavingTButton = swptb, 
                displayFmTButton = dfmtb,
                editCkTButton = ecktb
              }

-- ---------------------------------------------------------
-- Connect the GUI to the user events.
-- ----------------------------------------------------------
connectGui gui = 
 do
  onDestroy (window gui) mainQuit
  -- onDestroy (fmWindow gui)$ widgetHide (fmWindow gui)
  onDelete  (fmWindow gui) $ \event -> widgetHide (fmWindow gui) >> return True 
  
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
--  onToolButtonClicked (editCkTButton gui)    (editConfigurationKnowledge gui)
  
  widgetShowAll (window gui)   
  mainGUI

-------------------------------------------------------------------------------------
-- * GUI Events implementation 
     
-- ----------------------------------------------------------------------------------
-- starts the weaving process (or product derivation process) 
-- when the user clicks on the "generate products" button. 
-- ----------------------------------------------------------------------------------
weaveFiles gui = 
 do 
   -- retrieve the selected files and put them on a list.
   selectedFiles <- mapM fileChooserGetFilename [ rmFChooser  gui
                                                , ucmFChooser gui
                                                , cmFChooser  gui
                                                , fmFChooser  gui
                                                , pcFChooser  gui
                                                , ckFChooser  gui
                                                , outFChooser gui
                                                ]
   -- check if all files were selected....
   case selectedFiles of 
     [Just r, Just u, Just c, Just f, Just p, Just ck, Just o] -> do executeBuildingProcess gui (r, u, c, f, p, ck, o)
   
     -- ... if not, a message is displayed to the user 
     otherwise -> showDialog  (window gui) 
                              MessageError 
                              "Error on input files. Try checking these files before starting the weaving process."  

-- ------------------------------------------------------------------------------------
-- starts the file checker process, which updates 
-- the error list if any error was found. 
-- ------------------------------------------------------------------------------------
checkFiles gui store = 
 do
   r <- fileChooserGetFilename (rmFChooser gui)
   u <- fileChooserGetFilename (ucmFChooser gui)
   f <- fileChooserGetFilename (fmFChooser  gui)
   p <- fileChooserGetFilename (pcFChooser gui)
   c <- fileChooserGetFilename (ckFChooser gui)
   New.listStoreClear store
   executeFileChecker r rmSchema "Requirement model" store
   executeFileChecker u ucSchema "Use case model" store
   executeFileChecker p fcSchema "Instance model" store
   executeFileChecker c ckSchema "Configuration knowledge" store
   -- 
   -- checking a feature model is a bit more trick, since we
   -- accept different formats
   -- 
   case f of 
     Just fmFileName -> do 
            fm <- parseFeatureModel' fmFileName
            let fmErr = case fm of 
                          Core.Fail e -> [ErrorData "FeatureModel" e]
                          otherwise   -> []
            updateErrorStore store fmErr
     otherwise -> updateErrorStore store [ErrorData "FeatureModel" "Model not loaded."]       

-- -------------------------------------------------------------------------------------
-- displays the selected feature model. Note, the 
-- current implementation has a bug. It fails on the 
-- second time a feature model is displayed. It might be a 
-- gtk2HS problem. 
-- --------------------------------------------------------------------------------------
displayFeatureModel gui store = 
 do 
  f <- fileChooserGetFilename (fmFChooser gui)
  displayFeatureModel' f gui store

-- here we deal with the case where the 
-- feature model file was selected. 
displayFeatureModel' (Just fName) gui store = 
 do 
  f <-  parseFeatureModel' fName
  case f of 
   Core.Success fm -> 
       do  
         let t = feature2TreeNode (fmTree fm) 
         New.treeStoreClear store 
         New.treeStoreInsertTree store [] 0 t
         widgetShowAll (fmWindow gui)
   Core.Fail s -> 
       do showDialog (window gui) 
                     MessageError 
                     ("Error parsing feature model: " ++ s)

-- here we deal with the cases when the 
-- feature model file was not selected.
displayFeatureModel' Nothing gui store = 
 do showDialog (window gui) 
               MessageError 
               "Please, select a feature model."


-----------------------------------------------------------
-- auxiliarly functions for parsing feature models
-- TODO: this version supports just fmplugin and fmide.
-- TemplateHaskell may help here, creating a family of 
-- programs that supports different parsers.
-----------------------------------------------------------

supportedFmTypes = [ ("xml", FMPlugin), (".m" , FMIde) ]

parseFeatureModel' fmfile = 
 let p = [snd t | t <- supportedFmTypes , (fst t) `isSuffixOf` fmfile]
 in case p of 
  [x] -> (parseFeatureModel (fmfile, fmSchema) x)
  otherwise -> error "Error identifying the feature model type"


-- -------------------------------------------------------------------------------  
-- Execute the building process.
-- --------------------------------------------------------------------------------
executeBuildingProcess :: GUI -> (String, String, String, String, String, String, String) -> IO ()
executeBuildingProcess gui (rmFile, ucmFile, cmFile, fmFile, icFile, ckFile, outFile) = 
 do 
   -- parse results that should be checked if they are Core.Success.
   fmpr <- parseFeatureModel' fmFile 
   rmpr <- parseRequirementModel rmFile
   ucpr <- parseUseCaseFile ucmFile ucSchema
   cmpr <- parseComponentModel cmFile
   icpr <- parseInstanceConfiguration icFile 
   ckpr <- parseConfigurationKnowledge ckFile    
        
   case (fmpr, rmpr, ucpr, cmpr, icpr, ckpr) of 
     (Core.Success fm, Core.Success rm, Core.Success ucm, Core.Success cm, Core.Success icTree, Core.Success ck) -> 
         do 
           let fc  = FeatureConfiguration icTree
           let spl = SPLModel fm rm ucm cm 
           let product = build fm fc ck spl
           exportResult gui product

     otherwise -> putStrLn "Error on input files. Try to check the input file."
    
-- -------------------------------------------------------------------------
-- export the output files of a product generated from 
-- the results of a weaving process. 
-- ------------------------------------------------------------------------
exportResult gui p = 
 do
   odir        <- fileChooserGetCurrentFolder (outFChooser gui)
   (Just sdir) <- fileChooserGetCurrentFolder (sdFChooser gui)
   case odir of 
     Just odpath -> do 
         exportUcmToLatex (odpath ++ "/use-cases.tex") (ucm p)
         exportBuildFile  (odpath ++ "/build.lst") (components p)
         copySourceFiles odpath sdir (components p)                  
            
     Nothing -> showDialog (window gui) 
                MessageError 
               "Please, select an output directory."

-- ----------------------------------------------------------------------
-- Copy selected source files to the output directory
-- ----------------------------------------------------------------------
copySourceFiles out source [] = do return ()
copySourceFiles out source (x:xs) = 
 do 
  copySourceFile out source x
  copySourceFiles out source xs

copySourceFile out source f = 
 do 
  let l = length (Core.split '/' f)
  let d = fst $ splitAt (l-1) (Core.split '/' f)
  let p = concat $ map (++ "/") d   
  let cmd =  (copy ++ (source ++ f) ++ " " ++ (out ++ p)) 
  code <- System.system (copy ++ (source ++ "/" ++ f) ++ " " ++ (out ++ "/src/" ++ p))   
  case code of 
   ExitSuccess   -> print $ "[Hephaestus] -> File " ++ f ++ " exported to the output directory."
   ExitFailure n -> do 
    print $ "[Hepheastus] -> Creating directory " ++ (out ++ "/src/" ++ p)
    System.system(mkdir ++ out ++ "/src/" ++ p)
    print $ "[Hephaestus] -> Directory " ++ (out ++ "/src/" ++ p) ++ " created." 
    System.system (copy ++ (source ++ "/" ++ f) ++ " " ++ (out ++ "/src/" ++ p))  
    print $ "[Hephaestus] -> File " ++ f ++ " exported to the output directory." 
              
-- -----------------------------------------------------------------------
-- Exports a use case model as a latex document.
-- -----------------------------------------------------------------------
exportUcmToLatex f ucm = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (show (ucmToLatex ucm)))

-- --------------------------------------------------------------------
-- Exports the list of components as a build file. The format of this 
-- build file is similar to the Mobile Media build files.
-- -------------------------------------------------------------------
exportBuildFile f cs = 
 bracket (openFile f WriteMode)
         hClose
         (\h -> hPutStr h (concat [c ++ "\n" | c <- cs]))

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
                               ] (Core.createURI f)
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

  