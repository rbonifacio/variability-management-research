-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ComponentModel.Parsers.ParComponentModel where
import ComponentModel.Parsers.AbsComponentModel
import ComponentModel.Parsers.LexComponentModel
import ComponentModel.Parsers.ErrM
}

%name pComponentModel ComponentModel
%name pComponentMapping ComponentMapping
%name pRelativePath RelativePath
%name pListComponentMapping ListComponentMapping

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '=>' { PT _ (TS "=>") }
 '.' { PT _ (TS ".") }
 '/' { PT _ (TS "/") }
 ';' { PT _ (TS ";") }

L_ident  { PT _ (TV $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

ComponentModel :: { ComponentModel }
ComponentModel : ListComponentMapping { TComponentModel $1 } 


ComponentMapping :: { ComponentMapping }
ComponentMapping : Ident '=>' RelativePath { TComponentMapping $1 $3 } 


RelativePath :: { RelativePath }
RelativePath : Ident { BasicFilePath $1 } 
  | Ident '.' Ident { BasicFilePathExt $1 $3 }
  | Ident '/' RelativePath { ComposedFilePath $1 $3 }


ListComponentMapping :: { [ComponentMapping] }
ListComponentMapping : {- empty -} { [] } 
  | ComponentMapping { (:[]) $1 }
  | ComponentMapping ';' ListComponentMapping { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map prToken (take 4 ts))

myLexer = tokens
}

