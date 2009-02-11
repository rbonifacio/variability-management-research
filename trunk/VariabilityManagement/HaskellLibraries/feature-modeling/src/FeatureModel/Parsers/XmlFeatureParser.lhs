\begin{code}
module FeatureModel.Parsers.XmlFeatureParser where

import Text.XML.HXT.Arrow
import System.Environment

import qualified FeatureModel.Types as Base
import FeatureModel.Parsers.XmlFeatureModel 

instance XmlPickler XmlFeature where
	xpickle = xpFeature

instance XmlPickler XmlGroupFeature where 
	xpickle = xpGroup 
	
uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn (a, b, c, d, e) = fn a b c d e 

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn (a, b, c, d, e, f) = fn a b c d e f

xpFeature :: PU XmlFeature
xpFeature =
	xpElem "feature" $
	xpWrap (\ ((i,m1,m2),(n,c,g)) -> XmlFeature i m1 m2 n c g , 
                \t -> ((featureId t, cmin t, cmax t), (name t, children t, group t))) $
	xpPair (xpTriple (xpAttr "id" xpText)
	       (xpAttr "min" xpickle)
	       (xpAttr "max" xpickle))
	       (xpTriple (xpAttr "name" xpText)
	       (xpOption (xpList xpFeature)) 
	       (xpOption (xpGroup)))		 
			 
xpGroup :: PU XmlGroupFeature
xpGroup = 	
	xpElem "featureGroup" $
	xpWrap ( uncurry3 XmlGroupFeature, \ (XmlGroupFeature cmin cmax options) -> (cmin, cmax, options) ) $
	xpTriple ( xpAttr "min" xpickle ) 
	         ( xpAttr "max" xpickle ) 
	         ( xpList xpFeature )

parseFMPluginFile :: String -> Base.FeatureModel
parseFMPluginFile fileName = 
 do 
  [x] <- runX ( xunpickleDocument xpFeature [ (a_validate,v_0), (a_trace, v_1), (a_remove_whitespace,v_1), (a_preserve_comment, v_0)] fileName ) ;
  let ijk = xmlFeature2Feature x
  let abx = createFeatureModel ijk
  return abx
 
createFeatureModel :: Base.Feature -> Base.FeatureModel
createFeatureModel f = Base.FeatureModel f []				 
\end{code}