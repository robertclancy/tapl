module LambdaParser where

import Lambda
import Text.Parsec
import Text.Parsec.String (Parser)

type NParser = Parser NamedTerm

parse_term :: String -> Either ParseError NamedTerm
parse_term = parse term ""

term :: NParser
term = var <|> parens func

func :: NParser
func = abst <|> app

var :: NParser
var = do x <- identifier
         return $ NVar x

identifier :: Parser String
identifier = many1 letter

abst :: NParser
abst = do spaces
          _ <- string "lambda"
          skipMany1 space
          x <- identifier
          skipMany1 space
          t <- term
          spaces
          return $ NAbs x t

app :: NParser
app = do spaces
         t1 <- term
         skipMany1 space
         t2 <- term
         spaces
         return $ NApp t1 t2

parens :: Parser a -> Parser a
parens p = do _ <- char '('
              r <- p
              _ <- char ')'
              return r
