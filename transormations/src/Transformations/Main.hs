module Transformations.Main
where

import FeatureModel.Types

import FeatureModel.Parsers.GenericParser
import ConfigurationKnowledge.Parsers.GenericParser

import IO
import System

main = do 
  args <- getArgs
  if (length args == 3) 
   then do
    let fmFilePath = head args
    let icFilePath = head (tail args) 
    fm <- parseFeatureModel fmFilePath FMPlugin
    ic <- parseInstanceConfiguration icFilePath
    ck <- 
    putStrLn $ show (summary fm)
   else error "expecting Main <feature-model> <instance-model> <configuration-knowledge>"

