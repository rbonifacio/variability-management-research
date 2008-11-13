
module FeatureExpressionParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )
import UseCaseModel2Model
import UseCaseModel
import ProductLineModel
import FeatureModel

-- ---------------------------------------------------------------------------------
-- datatype used to represent the parser results.
-- 
-- it might be one of the folowings:
--   a) ParseExpressionResult
--   b) ParseTransformationResult
--   c) ParseError: indicates that an error has occurred while parsing
-- ---------------------------------------------------------------------------------
data ParseResult = ParseExpressionResult { expression :: FeatureExpression } 
                 | ParseTransformationResult { transformations :: [Model2Model] } 
 		 | ParseError  { parseErrorMessage :: String }



-- 
-- parses a char, but ignores spaces.
--
charWithoutSpace :: Char -> Parser Char
charWithoutSpace c =  
 do { skipMany space; 
  	  r <- char c; 
  	  skipMany space;
  	  return r
  	}	
  	
--
-- A simple parser for identifiers.
-- This parser recongnizes strings with the following 
-- pattern:
--
--  identifier :: char(alphanum)*
--
-- It is important to notice that this parser does not recongnize 
-- strings starting with a number.
--	 
identifier :: Parser String		
identifier = 
 do { c <- letter;
      s <- many alphaNum;
      return (c : s)
    }	
	

--
-- A parser for binary expressions envolving features.
-- 
parseBinaryExp :: (FeatureExpression -> FeatureExpression -> FeatureExpression)	-> Parser FeatureExpression
parseBinaryExp cons = 
 do { 
      skipMany space; char '('; skipMany space;	
      exp1 <- parseExp;
      char ','; 
      skipMany space;
      exp2 <- parseExp;
      skipMany space; char ')'; skipMany space;
      return (cons exp1 exp2)
  }

--
-- A parser for the not feature expression
--    
parseNotExp :: Parser FeatureExpression
parseNotExp = 
 do {
      skipMany space; char '('; skipMany space;			 
      exp1 <- parseExp;
      skipMany space; char ')'; skipMany space;
      return (NotExpression exp1)
 }    
	   		 
	   		 	
-- 
-- A parser for feature expressions.
-- This parser recongnize strings with the following
-- pattern:
-- parseExp :: And (parseExp, parseExp) |
--             Or (parseExp, parseExp)  |
--             Not (parseExp, parseExp  |
--			   identifier
-- 
-- Spaces are not considere.
--   			
parseExp :: Parser FeatureExpression
parseExp = 
 do { try  (string "And"); expression <- parseBinaryExp AndExpression; return expression } <|>
 do { try  (string "Or");  expression <- parseBinaryExp OrExpression;  return expression } <|>
 do { try  (string "Not"); expression <- parseNotExp;                  return expression } <|> 
 do { skipMany space; id1 <- identifier; skipMany space;               return (FeatureRef id1) }

--parseTransformationList  :: Parser [Model2Model UseCaseModel]
--parseTransformationList = 
-- do {
-- 	char '['; functions <- parseTransformations [];  char ']'; return functions
-- }
-- 
parseTransformations ::  Parser [Model2Model]
parseTransformations = 
 do { char '['; list <- sepBy1 parseTransformation separator; char ']'; return list}
 
separator :: Parser ()
separator = skipMany1 (space <|> char ',') 
 
parseTransformation :: Parser (Model2Model) 
parseTransformation = 
 do { try (string "addScenarios"); 
 	  char '('; 
 	  id1 <- identifier; 
 	  char ')'; 
 	  return (addScenariosM2M [id1])
 } <|>
 do { try (string "bindParameter"); 
 	  char '('; 
 	  id1 <- identifier; 
 	  char ','; 
 	  id2<-identifier ; 
 	  char ')'; 
 	  return (bindParametersM2M id1 id2)
 } 
-- <|>
-- do { try (string "evaluateAdvice");
--	  char '(';
--	  id1 <- identifier;
--	  char ')';
--	  return (evaluateAdviceM2M id1)
-- }			 

featureExpressionParser :: String -> ParseResult
featureExpressionParser str = 
 case (parse parseExp " " str) of
  Left err -> ParseError (show err)
  Right x -> ParseExpressionResult x   
  
transformationParser :: String -> ParseResult
transformationParser str = 
 case (parse parseTransformations " " str) of
  Left err -> ParseError (show err)
  Right x -> ParseTransformationResult x     

run :: Show a => Parser a -> String -> IO ()
run p input = 
 case (parse p " " input ) of 
  Right e -> print e
  Left err -> do { putStr "parse error at "; print err }
  

