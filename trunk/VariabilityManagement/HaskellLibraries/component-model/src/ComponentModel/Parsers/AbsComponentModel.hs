module ComponentModel.Parsers.AbsComponentModel where

-- Haskell module generated by the BNF converter

newtype Ident = Ident String deriving (Eq,Ord,Show)
data ComponentModel =
   TComponentModel [ComponentMapping]
  deriving (Eq,Ord,Show)

data ComponentMapping =
   TComponentMapping Ident RelativePath
  deriving (Eq,Ord,Show)

data RelativePath =
   BasicFilePath Ident
 | BasicFilePathExt Ident Ident
 | ComposedFilePath Ident RelativePath
  deriving (Eq,Ord,Show)

