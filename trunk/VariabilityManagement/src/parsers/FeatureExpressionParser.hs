
module FeatureExpressionParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )

import FeatureModel


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
identifier = do {
				c <- letter;
				s <- many alphaNum;
				return (c : s)
	   		 }		

-- 
-- A parser for feature expressions.
-- This parser recongnize strings with the followin
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
 do { try  (string "And");
      skipMany space; char '('; skipMany space;			 
      exp1 <- parseExp;
      char ',';
      exp2 <- parseExp;
      skipMany space; char ')'; skipMany space;
      return (AndExpression exp1 exp2) 
    } <|>
 do { try  (string "Or");
      skipMany space; char '('; skipMany space;			 
      exp1 <- parseExp;
      char ',';
      exp2 <- parseExp;
      skipMany space; char ')'; skipMany space;
      return (OrExpression exp1 exp2) 
    } <|>
 do { try  (string "Not");
      skipMany space; char '('; skipMany space;			 
      exp1 <- parseExp;
      skipMany space; char ')'; skipMany space;
      return (NotExpression exp1) 
    }<|> 
 do {skipMany space; id1 <- identifier; skipMany space; return (FeatureRef id1) }


featureExpressionParser :: String -> FeatureExpression
featureExpressionParser str = 
 case (parse parseExp " " str) of
  Left err -> error (show err)
  Right x -> x   

run :: Show a => Parser a -> String -> IO ()
run p input = 
 case (parse p " " input ) of 
  Left err -> do { putStr "parse error at "; print err }
  Right x  -> print x

