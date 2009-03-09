\begin{code}

module FeatureModel.Parsers.FMIde.FMIde2FeatureModel where

import FeatureModel.Types
import FeatureModel.Parsers.FMIde.AbsFMIde


-- | Translate a FMIde Grammar into an instance of the 
--  FeatureModel data type.

grammarToFeatureModel :: Grammar -> FeatureModel
grammarToFeatureModel (TGrammar (x:xs)) = 
 case x of 
  -- we expecte a TBase production, since...
  TBaseProduction prod ts prodName -> 
   let 
    gram  = TGrammar (x:xs)
    froot = productionToFeature gram 
   in FeatureModel { fmRoot = (productionToFeature gram Mandatory BasicFeature x), 
                     fmConstraints = []
                   }
  -- the production for the root feature must be a BaseProduction
  otherwise -> error "Expecting a base production for the root feature." 

productionToFeature :: Grammar -> FeatureType -> GroupType -> Production -> Feature
productionToFeature gram ft gt (TBaseProduction prod ts prodName) = bpToFeature gram ft gt (TBaseProduction prod ts prodName)
productionToFeature gram ft gt (TAltProduction prod os) = apToFeature gram ft gt (TAltProduction prod os)

bpToFeature :: Grammar -> FeatureType -> GroupType -> Production -> Feature
bpToFeature gram ft gt (TBaseProduction prod ts prodName) = 
 case prodName of 
  -- productions in the form A : B C :: D are transformed into the feature D [B, C] 
  TProdName pn -> Feature {
                    fId = idToString (pn),
                    fName = idToString (pn),
                    fType = ft,
                    groupType = gt,
                    children = [termToFeature gram t | t <- ts],
                    properties = []
                  } 
  -- productions in the form A : B C :: _A are transformed into the feature A [B, C] 
  TProdNameL pn -> Feature {
                     fId = idToString (key prod),
                     fName = idToString (key prod),
                     fType = ft,
                     groupType = gt,
                     children = [termToFeature gram t | t <- ts],
                     properties = []
                    } 
  -- productions in the form A : B :: B_ are transformed into the feature A [B] 
  TProdNameR pn -> Feature {
                    fId = idToString (key prod),
                    fName = idToString (key prod),
                    fType = ft,
                    groupType = gt,
                    children = map (productionToFeature gram Mandatory AlternativeFeature) (findProduction (pn) gram) ,
                    properties = []
                  }       

apToFeature :: Grammar -> FeatureType -> GroupType -> Production -> Feature
apToFeature gram ft gt (TAltProduction prod os) = 
 Feature { 
  fId = idToString (key prod),
  fName = idToString (key prod),
  fType = ft,
  groupType = gt,
  children = [optionToFeature gram o | o <- os],
  properties = []
 }

termToFeature :: Grammar -> Term -> Feature
termToFeature gram term = 
 let 
   fDef = termToFeatureDef term
   ps = findProduction (key term) gram
 in case ps of 
  -- no production related to the term
  []  -> Feature { 
              fId = idToString (key term),
              fName = idToString (key term),
              fType = fst fDef,
              groupType = snd fDef, 
              children = [],
              properties = []
         }
  -- one production related to the term
  [x] -> productionToFeature gram (fst fDef) (snd fDef) x
  -- oops, more than one production with the same name
  (x:xs) -> error ("Expecting just one production labeled as: " ++ (idToString (key x))) 
 
termToFeatureDef :: Term -> (FeatureType, GroupType)
termToFeatureDef (TTerm x) = (Mandatory, BasicFeature)
termToFeatureDef (TOptionalTerm x) = (Optional, BasicFeature)
termToFeatureDef (TOrTerm x) = (Mandatory, OrFeature)
termToFeatureDef (TXorTerm x) = (Mandatory, AlternativeFeature)

optionToFeature :: Grammar -> Option -> Feature
optionToFeature gram opt = 
 let 
  ps = findProduction (key opt) gram
 in case ps of 
  -- first case, there is no production 
  [] -> Feature { fId = idToString (key opt),
                  fName = idToString (key opt),
                  fType = Optional,
                  groupType = BasicFeature,
                  children = [],
                  properties = []
        }
  -- second case, there is one production
  [x] -> productionToFeature gram Optional BasicFeature x
  -- oops, more than one production with the same name 
  (x:xs) -> error ("Expecting just one production labeled as: " ++ (idToString (key x)))  
                  
findProduction :: Ident -> Grammar -> [Production]
findProduction i (TGrammar ps) = [p | p <- ps, ((key p) == i)] 

class Identifier a where 
 key :: a -> Ident

instance Identifier Production where
 key (TBaseProduction prod ts prodName) = key prod
 key (TAltProduction prod os) = key prod 

instance Identifier BaseProd where 
 key (TBaseProd i) = i

instance Identifier AltProd where 
 key (TAltProd i)  = i

instance Identifier ProdName where 
 key (TProdName i)  = i
 key (TProdNameL i) = i
 key (TProdNameR i) = i

instance Identifier Term where 
 key (TTerm i)         = i
 key (TOptionalTerm i) = i
 key (TOrTerm i)       = i
 key (TXorTerm i)      = i

instance Identifier Option where 
 key (TOption i) = i

idToString :: Ident -> String
idToString (Ident s) = s



\end{code}