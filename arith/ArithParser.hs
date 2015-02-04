module ArithParser (parse_term) where

import Arith
import Text.Parsec
import Text.Parsec.String (Parser)

type AParser = Parser Term

parse_term :: String -> Either ParseError Term
parse_term = parse term ""

term :: AParser
term = atom <|> expr

atom :: AParser
atom = try tru <|> try fls <|> zero <?> "atom"

expr :: AParser
expr = parens application

application :: AParser
application = if_app <|> succ_app <|> pred_app <|> is_zero_app <?> "function application"

succ_app :: AParser
succ_app = func_app "succ" Succ

pred_app :: AParser
pred_app = func_app "pred" Pred

is_zero_app :: AParser
is_zero_app = func_app "zero?" IsZero

func_app :: String -> (Term -> Term) -> AParser
func_app func_name func = do string func_name
                             skipMany1 space
                             t <- term
                             return $ func t

if_app :: AParser
if_app = do string "if"
            skipMany1 space
            test <- term
            skipMany1 space
            t <- term
            skipMany1 space
            f <- term
            return $ If test t f

tru :: AParser
tru = string "#t" >> return Tru

fls :: Parser Term
fls = string "#f" >> return Fls

zero :: Parser Term
zero = string "#z" >> return Zero

parens :: Parser a -> Parser a
parens p = do char '('
              r <- p
              char ')'
              return r
