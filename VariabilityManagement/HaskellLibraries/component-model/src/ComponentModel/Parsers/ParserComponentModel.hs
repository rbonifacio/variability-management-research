module ComponentModel.Parsers.ParserComponentModel
where 

import BasicTypes

import ComponentModel.Parsers.AbsComponentModel
import ComponentModel.Parsers.SkelComponentModel
import ComponentModel.Parsers.ErrM
import ComponentModel.Parsers.LexComponentModel
import ComponentModel.Parsers.ParComponentModel

import qualified ComponentModel.Types as T

parseComponentModel fileName = do 
 x <- readFile (fileName) 
 let cm = parseResult (pComponentModel (myLexer x))
 return cm

parseResult (Ok g)  = Success (translateModel g)
parseResult (Bad s) = Fail s

translateModel :: ComponentModel -> T.ComponentModel
translateModel (TComponentModel cs) = map translateMapping cs 

translateMapping :: ComponentMapping -> T.ComponentMapping 
translateMapping (TComponentMapping (Ident i) p) = (i, path p)

path :: RelativePath -> String
path (BasicFilePath (Ident n) (Ident e)) = n ++ "." ++ e
path (ComposedFilePath (Ident i) p ) = i ++ "/" ++ path p