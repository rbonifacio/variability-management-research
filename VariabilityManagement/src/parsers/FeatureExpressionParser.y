{
module FeatureExpressionParser (parseExpression) where

%name parseExpression
%tokentype { Token }
%error { parseError }

%token
      'and'       	{ TokenAnd }
      'or'          { TokenOr }
      'not'         { TokenNot }
      '('           { TokenOB }
      ')'           { TokenCB }
      
%%


Exp   : Exp '&' Exp  			{ AndExpression $1 $3 }
      | Exp '|' Exp  			{ OrExpression $1 $3 }
      | '!' Exp					{ NotExpression $2 }  
      | Exp1         			{ Exp1 $1 }

Exp1  : id						{ FeatureRef $1 }
      | '(' Exp ')'             { Brack $2 }      

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenAnd
      | TokenOr
      | TokenNot
      | TokenOB
      | TokenCB
 deriving Show
 
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('|':cs) = TokenOr : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

}