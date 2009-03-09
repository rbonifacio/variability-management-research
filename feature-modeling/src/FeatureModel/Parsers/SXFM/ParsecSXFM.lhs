\begin{code}

module FeatureModel.Parsers.SXFM.ParsecSXFM
where 

import FeatureModel.Types 

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

simple :: Parser Char
simple = letter

parseFeatureModel :: Parser FeatureModel 
parseFeatureModel = 
 do { string "<feature_model name=\""  ; skipMany space ;
      parseName                        ; skipMany space ;
      string"\">" ;                    ; skipMany space ;
      string "<feature_tree>"          ; skipMany space ;
      r <- parseRootFeature            ; skipMany space ;
      string "</feature_tree>"         ; skipMany space ;
      constraints <- parseConstraints  ; skipMany space ;
      string "</feature_model>"        ; skipMany space ;
      return FeatureModel { fmRoot = r, fmConstraints = constraints } 
 }

parseRootFeature :: Parser Feature 
parseRootFeature = 
  do { string ":r"       ; skipMany space ; 
       n <- parseName    ; skipMany space ;
       i <- option n parseId ;  
       c <- option [] (parseChildren i) ;
       return Feature { fId = i ,
                        fName = n ,
                        fType = Mandatory ,
                        groupType = BasicFeature ,
                        children = c ,
                        properties = []
                      }
  } 

-- parseChildren :: Int -> Parser [Feature]
-- parseChildren x = 
--  do { skipMany space;
--       n <- parseCountTabs 0 ; skipMany space; 
--       if n == (x+1)
--         then
--          do {  
--           c  <- (parseChild n) ;
--           cs <- (parseChildren x) ; 
--           return (c : cs) 
--          }
--         else
--          do return []
--  }


parseChildren :: String -> Parser [Feature]
parseChildren f = 
 do { skipMany space;
      char '[' ;  
      c  <- many parseChild  ;
      char ']' ; skipMany space; ;
      return c 
    }
 <|> 
 do { skipMany space;
      g <- parseGroup f;
      return g ;
 }

parseChild :: Parser Feature 
parseChild = 
 do { t <- parseFeatureType ; skipMany space ;
      n <- parseName ; skipMany space ;
      i <- option n parseId ; skipMany space ;
      c <- option [] (parseChildren i) ;
      return Feature { fId = i , 
                       fName = n ,
                       fType = t ,
                       groupType = BasicFeature ,
                       children = c ,
                       properties = []
                     } 
    }

parseGroup :: String -> Parser [Feature] 
parseGroup n = 
 do { char '<' ; skipMany space ;
      t <- parseGroupType ; skipMany space ;
      char '[' ; skipMany space ;
      c <- option [] (many parseOption) ;
      char ']' ; skipMany space ;
      char '>' ; skipMany space ;
      return [Feature { fId = n ++ "_g" , 
                        fName = n ++ "_g" ,
                        fType = Mandatory ,
                        groupType = t ,
                        children = c ,
                        properties = [] 
                   } 
             ] 
    }

parseOption :: Parser Feature 
parseOption = 
 do { string ":" ; skipMany space   ; 
      n <- parseName ; skipMany space ;
      i <- option n parseId ; skipMany space ;
      c <- option [] (parseChildren i) ; skipMany space ; skipMany (char '\r') ; skipMany (char '\n') ;
      return Feature { fId = i , 
                       fName = n ,
                       fType = Optional ,
                       groupType = BasicFeature ,
                       children = c ,
                       properties = [] 
                     }  
  }

parseFeatureType :: Parser FeatureType 
parseFeatureType = 
 do { try (string ":o") ;
      return Optional ;
    }
 <|>
 do { try (string ":m") ;
      return Mandatory ;  
    } 

parseGroupType :: Parser GroupType
parseGroupType = 
 do { try (string ":g[1,1]") ; 
      return AlternativeFeature ;
 }
 <|>
 do { string ":g[1,*]" ; 
      return OrFeature ;
 }
 

parseConstraints :: Parser [Constraint]
parseConstraints = 
 do 
  { string "<constraints>" ; skipMany space ;
    cs <- many parseConstraint ; 
    string "</constraints>" ; skipMany space  ;
    return cs;
  }

parseConstraint :: Parser Constraint 
parseConstraint = 
 do { string "implies" ; skipMany space; 
      char '('  ; skipMany space;
      lhs <- parseExp ; skipMany space;
      char ',' ; skipMany space;
      rhs <- parseExp ;  skipMany space;
      char ')' ; skipMany space;
      return Constraint { constraintType = Implies, 
                          constraintLHSExp = lhs , 
	                  constraintRHSExp = rhs 
                        }
 }

parseExp :: Parser FeatureExpression
parseExp = 
 do { string "True" ; skipMany space; 
      return expTrue ;  
    } 
 <|>
 do { string "False" ; skipMany space; 
      return expFalse ;  
    }
 <|> 
 do { string "Not" ; skipMany space ; 
      char '(' ; skipMany space ; 
      e <- parseExp ; skipMany space; 
      char ')' ; 
      return (Not e)  
    } 
 <|>
 do { string "And" ; skipMany space ; 
      char '(' ; skipMany space ; 
      e1 <- parseExp ; skipMany space; 
      char ',' ; skipMany space; 
      e2 <- parseExp ; skipMany space; 
      char ')' ; 
      return (And e1 e2) 
 }
 <|>
 do { string "Or" ; skipMany space ; 
      char '(' ; skipMany space ; 
      e1 <- parseExp ; skipMany space; 
      char ',' ; skipMany space; 
      e2 <- parseExp ; skipMany space; 
      char ')' ; 
      return (Or e1 e2) 
 }
 <|>
 do { idr <- parseName ; skipMany space; 
      return (FeatureRef idr)  
    } 
 
parseId :: Parser String 
parseId = do { char '(' ; skipMany space ;
               i <- parseName ; skipMany space ;
               char ')' ; skipMany space ;
               return i 
             }
parseName :: Parser String
parseName = do { c <- letter ; 
                 s <- many (alphaNum <|> char '_' <|> char ' ');
                 return (c : s)
               }        

parseCountTabs :: Int -> Parser Int 
parseCountTabs n = 
 do {
    try (char '\t') ;
    parseCountTabs (n+1)
 }
 <|> do { return n } 

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x


\end{code}