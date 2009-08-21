module ComponentModel.Parsers.SkelComponentModel where

-- Haskell module generated by the BNF converter

import ComponentModel.Parsers.AbsComponentModel
import ComponentModel.Parsers.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transComponentModel :: ComponentModel -> Result
transComponentModel x = case x of
  TComponentModel componentmappings  -> failure x


transComponentMapping :: ComponentMapping -> Result
transComponentMapping x = case x of
  TComponentMapping id relativepath  -> failure x


transRelativePath :: RelativePath -> Result
transRelativePath x = case x of
  BasicFilePath id  -> failure x
  BasicFilePathExt id0 id  -> failure x
  ComposedFilePath id relativepath  -> failure x



