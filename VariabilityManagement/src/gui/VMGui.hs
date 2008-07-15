module Main where

import Maybe

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.ModelView as New

import Text.XML.HXT.Arrow
import System.Environment

import XmlUseCaseParser
import XmlFeatureModel
import XmlFeatureParser

data UseCaseDocument = UseCaseDocument { path :: String }

main :: IO ()
main = do
  initGUI
  
  Just xml <- xmlNew "vm.glade"  
  
  window   <- xmlGetWidget xml castToWindow "mainWindow"
  onDestroy window mainQuit
  
  useCaseList <- xmlGetWidget xml castToTreeView "useCasesList"
  useCaseStore <- createUseCaseStore
  New.treeViewSetModel useCaseList useCaseStore
  setupUseCaseView useCaseList useCaseStore
  
  featureModelEntry <- xmlGetWidget xml castToEntry "featureModelEntry"
   
  addUseCaseButton <- xmlGetWidget xml castToButton "addUseCaseButton"
  addUseCaseButton `onClicked` openSelectFileDialog useCaseStore window 
  
  selectFMButton <- xmlGetWidget xml castToButton "selectFeatureModelButton"
  selectFMButton `onClicked` openSelectFMDialog featureModelEntry window
  
  cancelButton <- xmlGetWidget xml castToButton "cancelButton"
  onClicked cancelButton $ do widgetDestroy window
  
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

-- 
-- This function creates a store with the selected use case documents. 
-- Such a store is used as the UC model element of the GUI.
-- 
createUseCaseStore = New.listStoreNew [ ]
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
  

  