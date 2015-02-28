module Language.SimplyTyped.Parser (term, readTerm) where

import Language.SimplyTyped.Syntax
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

-- Standard parser
readTerm :: String -> Either ParseError (Term String)
readTerm = parse term ""

-- The Parser

-- Terms
term :: Parser (Term String)
term = whiteSpace >> app_e

true :: Parser (Term String)
true = reserved "true" >> return TmTrue

false :: Parser (Term String)
false = reserved "false" >> return TmFalse

var :: Parser (Term String)
var = fmap TmVar identifier

abs_p :: Parser (Term String)
abs_p = do _ <- symbol "\\"
           x <- identifier
           _ <- symbol ":"
           ty <- type_e
           _ <- symbol "."
           b <- term
           return $ TmAbs x ty b

if_p :: Parser (Term String)
if_p = do reserved "if"
          b <- app_parens
          x <- app_parens
          y <- app_parens
          return $ TmIf b x y

-- Application Expressions
app_e :: Parser (Term String)
app_e = chainl1 app_parens app_op

app_op :: Parser (Term String -> Term String -> Term String)
app_op = whiteSpace >> return TmApp

app_parens :: Parser (Term String)
app_parens = abs_p <|> if_p <|> true <|> false <|> var <|> parens app_e

-- Type Expressions
type_e :: Parser Type
type_e = chainr1 type_parens type_arr

type_parens :: Parser Type
type_parens = type_atom <|> parens type_e

type_atom :: Parser Type
type_atom = reserved "Bool" >> return TyBool

type_arr :: Parser (Type -> Type -> Type)
type_arr = reservedOp "->" >> return TyArr

-- Lexer

language :: P.LanguageDef st
language = emptyDef {
                    P.reservedNames = ["true", "false", "if", "Bool"],
                    P.reservedOpNames = ["->"]
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
