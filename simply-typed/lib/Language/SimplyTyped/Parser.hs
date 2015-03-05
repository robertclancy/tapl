module Language.SimplyTyped.Parser (term, readTerm) where

import Language.SimplyTyped.Syntax
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

type PTerm = Term String String

-- Standard parser
readTerm :: String -> Either ParseError PTerm
readTerm = parse term ""

-- The Parser

-- Terms
term :: Parser PTerm
term = do whiteSpace
          t <- term_p
          eof
          return t

term_p :: Parser PTerm
term_p = let_p <|> abs_p <|> if_p <|> app_e

atom :: Parser PTerm
atom = true <|> false <|> var

true :: Parser PTerm
true = reserved "true" >> return TmTrue

false :: Parser PTerm
false = reserved "false" >> return TmFalse

var :: Parser PTerm
var = fmap TmVar identifier

let_p :: Parser PTerm
let_p = do reserved "let"
           x <- identifier
           reservedOp "="
           val <- term_p
           reserved "in"
           body <- term_p
           return $ TmLet x val body

abs_p :: Parser PTerm
abs_p = do _ <- symbol "\\"
           x <- identifier
           _ <- symbol ":"
           ty <- type_e
           _ <- symbol "."
           b <- term_p
           return $ TmAbs x ty b

if_p :: Parser PTerm
if_p = do reserved "if"
          b <- term_p
          reserved "then"
          x <- term_p
          reserved "else"
          y <- term_p
          return $ TmIf b x y

-- Application Expressions
app_e :: Parser PTerm
app_e = chainl1 app_parens app_op

app_op :: Parser (PTerm -> PTerm -> PTerm)
app_op = whiteSpace >> return TmApp

app_parens :: Parser PTerm
app_parens = atom <|> parens (let_p <|> abs_p <|> if_p <|> app_e)

-- Type Expressions
type_e :: Parser Type
type_e = chainr1 type_parens type_arr

type_parens :: Parser Type
type_parens = type_atom <|> parens type_e

type_atom :: Parser Type
type_atom = symbol "Bool" >> return TyBool

type_arr :: Parser (Type -> Type -> Type)
type_arr = reservedOp "->" >> return TyArr

-- Lexer

language :: P.LanguageDef st
language = emptyDef {
                    P.reservedNames = ["true", "false", "if", "then", "else", "let", "in"],
                    P.reservedOpNames = ["->", "="]
}

lexer      = P.makeTokenParser language
parens :: Parser a -> Parser a
parens     = P.parens lexer
identifier :: Parser String
identifier = P.identifier lexer
reserved :: String -> Parser ()
reserved   = P.reserved lexer
symbol :: String -> Parser String
symbol     = P.symbol lexer
reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer
whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer
