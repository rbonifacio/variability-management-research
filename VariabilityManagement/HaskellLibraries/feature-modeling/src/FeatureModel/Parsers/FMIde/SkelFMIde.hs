module FeatureModel.Parsers.FMIde.SkelFMIde where

-- Haskell module generated by the BNF converter

import FeatureModel.Parsers.FMIde.AbsFMIde
import FeatureModel.Parsers.FMIde.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident str  -> failure x


transGrammar :: Grammar -> Result
transGrammar x = case x of
  TGrammar productions  -> failure x


transProduction :: Production -> Result
transProduction x = case x of
  TBaseProduction baseprod terms prodname  -> failure x
  TAltProduction altprod options  -> failure x


transBaseProd :: BaseProd -> Result
transBaseProd x = case x of
  TBaseProd id  -> failure x


transAltProd :: AltProd -> Result
transAltProd x = case x of
  TAltProd id  -> failure x


transProdName :: ProdName -> Result
transProdName x = case x of
  TProdName id  -> failure x
  TProdNameL id  -> failure x
  TProdNameR id  -> failure x


transTerm :: Term -> Result
transTerm x = case x of
  TTerm id  -> failure x
  TOptionalTerm id  -> failure x
  TOrTerm id  -> failure x
  TXorTerm id  -> failure x


transOption :: Option -> Result
transOption x = case x of
  TOption id  -> failure x



