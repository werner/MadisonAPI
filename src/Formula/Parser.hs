-- Based on https://wiki.haskell.org/Parsing_a_simple_imperative_language
module Formula.Parser (parseString) where

import System.IO
import Control.Monad
import Control.Exception
import Formula.Types
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Prim as Prim
import qualified Text.ParserCombinators.Parsec.Token as Token

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":=", "^", "==", "!=",
                                      "<=", ">=", "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
function   = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
                                   
semi       = Token.semi       lexer
comma      = Token.comma      lexer
whiteSpace = Token.whiteSpace lexer
number     = Token.naturalOrFloat lexer

formulaParser :: Parser Stmt
formulaParser = whiteSpace >> statement

statement :: Parser Stmt
statement = parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- sepBy1 statement' semi
     return $ head list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> Prim.try functionStmt
           <|> aExprStmt
           <|> bExprStmt

functionStmt :: Parser Stmt
functionStmt = do
    name <- function
    args <- parens $ sepBy1 aExpression comma
    return $ Function name args

aExprStmt :: Parser Stmt
aExprStmt = do
   expr <- aExpression
   return $ StmtAExpr expr

bExprStmt :: Parser Stmt
bExprStmt = do
   expr <- bExpression
   return $ StmtBExpr expr

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return  Neg                     )          ]
             , [Infix  (reservedOp "^"   >> return (ABinary Power           )) AssocLeft]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply        )) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide          )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add             )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract        )) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "not" >> return  Not             )          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> fmap Var identifier
     <|> fmap NumConst number

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">"  >> return Greater)
         <|> (reservedOp "<"  >> return Less)
         <|> (reservedOp "==" >> return Equal)
         <|> (reservedOp ">=" >> return GreaterOrEqual)
         <|> (reservedOp "<=" >> return LessOrEqual)
         <|> (reservedOp "!=" >> return NotEqual)

parseString :: String -> Stmt
parseString str =
  case parse (formulaParser <* eof) "formula" str of
    Left e  -> throw(SyntaxError $ error $ show e)
    Right r -> r
