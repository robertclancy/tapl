module Language.Inference.Parser (term, readTerm) where

import Language.Inference.Syntax
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

-- Standard parser
readTerm :: String -> Either ParseError Term
readTerm = parse term ""

-- The Parser

-- Terms
term :: Parser Term
term = do whiteSpace
          t <- term_p
          eof
          return t

term_p :: Parser Term
term_p = abs_p <|> succ_p <|> rec_p <|> if_p <|> app_e

atom :: Parser Term
atom = bool <|> zero <|> var

bool :: Parser Term
bool = true <|> false

true :: Parser Term
true = reserved "true" >> return TmTrue

false :: Parser Term
false = reserved "false" >> return TmFalse

zero :: Parser Term
zero = reserved "0" >> return TmZero

var :: Parser Term
var = fmap TmVar identifier

abs_p :: Parser Term
abs_p = do _ <- symbol "\\"
           x <- identifier
           _ <- symbol "."
           b <- term_p
           return $ TmAbs x b

if_p :: Parser Term
if_p = do reserved "if"
          b <- term_p
          reserved "then"
          x <- term_p
          reserved "else"
          y <- term_p
          return $ TmIf b x y

-- Application Expressions
app_e :: Parser Term
app_e = chainl1 app_parens app_op

app_op :: Parser (Term -> Term -> Term)
app_op = whiteSpace >> return TmApp

app_parens :: Parser Term
app_parens = atom <|> parens (term_p)

-- Expressions that could be parsed as function calls
succ_p :: Parser Term
succ_p = do reserved "succ"
            t <- app_parens
            return $ TmSucc t

rec_p :: Parser Term
rec_p = do reserved "rec"
           z <- app_parens
           f <- app_parens
           return $ TmRec z f

-- Lexer

language :: P.LanguageDef st
language = emptyDef {
                    P.reservedNames = ["true", "false", "if", "then", "else", "let", "in", "succ", "rec", "0"],
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
