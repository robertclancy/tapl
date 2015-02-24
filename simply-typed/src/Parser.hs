module Parser (term) where

import Terms
import Text.Parsec
import Text.Parsec.String (Parser)

type_e :: Parser Type
type_e = chainr1 type_parens type_arr

type_parens :: Parser Type
type_parens = type_atom <|> parens (do spaces
                                       e <- type_e
                                       spaces
                                       return e)

type_atom :: Parser Type
type_atom = string "Bool" >> return TyBool

type_arr :: Parser (Type -> Type -> Type)
type_arr = do spaces
              _ <- string "->"
              spaces
              return TyArr

parens :: Parser a -> Parser a
parens p = do _ <- char '('
              r <- p
              _ <- char ')'
              return r

term :: Parser (Term String)
term = spaces >> app_e

identifier :: Parser String
identifier = many1 letter

atom :: Parser (Term String)
atom = true <|> false

true :: Parser (Term String)
true = string "true" >> return TmTrue

false :: Parser (Term String)
false = string "false" >> return TmFalse

var :: Parser (Term String)
var = fmap TmVar identifier

abs_p :: Parser (Term String)
abs_p = do _ <- char '\\'
           x <- identifier
           spaces
           _ <- char ':'
           spaces
           ty <- type_e
           _ <- char '.'
           spaces
           b <- term
           return $ TmAbs x ty b

if_p :: Parser (Term String)
if_p = do _ <- string "if"
          spaces
          b <- app_parens
          spaces
          x <- app_parens
          spaces
          y <- app_parens
          return $ TmIf b x y

app_e :: Parser (Term String)
app_e = chainl1 app_parens app_op

app_op :: Parser (Term String -> Term String -> Term String)
app_op = spaces >> return TmApp

app_parens :: Parser (Term String)
app_parens = abs_p <|> try if_p <|> try true <|> try false <|> var <|>
             parens (do spaces
                        e <- app_e
                        spaces
                        return e)
