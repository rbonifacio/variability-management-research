
module FeatureExpressionParser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle )



simple :: Parser Char
simple = letter

openClose :: Parser Char
openClose = 
 do {							-- do notation used for sequencing "()"
 	char '('
   ;char ')'  	
 }
 
parens :: Parser () 
parens = 						-- accept "", "()", "(()(()))"
 do {
 	  char '('
 	; parens
 	; char ')'
 	; parens
 }
 <|> return()
 
nesting :: Parser Int 			-- the result is an Int value
nesting = 
  do {
 		  char '('
 	    ; n <- nesting
 	    ; char ')'
 	    ; m <- nesting
 	    ; return (max (n+1) m)  
  }
  <|> return 0
 	 
word :: Parser String				-- parser for char+ (many1; many for char*)
word = many1 letter	<?> "word"		-- this is possible through combinators
									-- <?> "word" improves the error messages
--
-- interesting example
-- Rodrigo Bonifacio estudando parsec, a parser generator in haskell.
--
sentence :: Parser [String]			-- returns a list of strings of a sentence
sentence = 
 do {
	  words <- sepBy1	word separator	-- parse a sequence of paresers separated by some separator
	; oneOf ".?!" <?> "end of sentence"	-- should finished with one of "?.!"
	; return words     
 }
 
separator :: Parser ()
separator = skipMany1 (space <|> char ',' <?> " ")

lexer :: TokenParser ()
lexer  = makeTokenParser 
         (haskellDef
         { reservedOpNames = ["or","and","not"]
         }
         
whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
semi      = P.semi lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer         
 

expr    :: Parser Integer
expr    = buildExpressionParser table factor <?> "expression"

table   = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
          ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
          ]          
        where
          op s f assoc = Infix (do{ reservedOp s; return f} <?> "operator") assoc

factor  =   parens expr
        <|> natural
        <?> "simple expression"          

run :: Show a => Parser a -> String -> IO ()
run p input = 
 case (parse p " " input ) of 
  Left err -> do { putStr "parse error at "; print err }
  Right x  -> print x

