module FeatureModel.Parsers.GenericParser
where 

import FeatureModel.Types

-- modules related to the FMPlugin parser
import FeatureModel.Parsers.FMPlugin.XmlFeatureParser 
import FeatureModel.Parsers.FMPlugin.XmlFeatureModel (xmlFeature2FeatureTree) 

-- modules related to the FMIde parser
import FeatureModel.Parsers.FMIde.FMIde2FeatureModel
import FeatureModel.Parsers.FMIde.AbsFMIde
import FeatureModel.Parsers.FMIde.SkelFMIde
import FeatureModel.Parsers.FMIde.ErrM
import FeatureModel.Parsers.FMIde.LexFMIde
import FeatureModel.Parsers.FMIde.ParFMIde

-- modules related to the FMGrammar parser
import qualified FeatureModel.Parsers.FMGrammar.Grammar2FeatureModel as GFMG
import qualified FeatureModel.Parsers.FMGrammar.LexFMGrammar as LFMG
import qualified FeatureModel.Parsers.FMGrammar.SkelFMGrammar as SFMG
import qualified FeatureModel.Parsers.FMGrammar.AbsFMGrammar as AFMG
import qualified FeatureModel.Parsers.FMGrammar.ParFMGrammar as PFMG 
import qualified FeatureModel.Parsers.FMGrammar.ErrM as EFMG

-- modules related to the SXFM parser
import  qualified FeatureModel.Parsers.SXFM.ParsecSXFM as ParsecSXFM

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

import Text.XML.HXT.Arrow

data FmFormat = FMPlugin | FMIde | FMGrammar | SXFM

genericParser fileName format = do
 x <- readFile (fileName) 
 case (format) of 
  FMPlugin -> do
    fm <- translateFMPToFm fileName
    return fm
   
  FMIde -> do
    let fm = translateFMIdeToFm (pGrammar (myLexer x))
    return fm

  FMGrammar -> do 
    let fm = translateFMGrammarToFm (PFMG.pFMGrammar (PFMG.myLexer x))
    return fm 

  SXFM  -> do
    r <- parseFromFile ParsecSXFM.parseFeatureModel fileName ; 
    case (r) of
      Left err  -> error (show err)
      Right f  -> do let fm = f
                     return fm

translateFMIdeToFm (Ok g)  = grammarToFeatureModel g
translateFMIdeToFm (Bad s) = error s

translateFMGrammarToFm (EFMG.Ok g) = GFMG.grammarToFeatureModel g  
translateFMGrammarToFm (EFMG.Bad s) = error s

translateFMPToFm s = 
 do
      [x] <- runX ( xunpickleDocument xpFeature [ (a_validate,v_0)
                                      , (a_trace, v_1)
                                      , (a_remove_whitespace,v_1)
                                      , (a_preserve_comment, v_0)
                                      ] s);
      return FeatureModel { fmTree = (xmlFeature2FeatureTree x), fmConstraints = [] }
