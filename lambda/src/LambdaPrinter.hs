module LambdaPrinter where

import Lambda
import Text.PrettyPrint

pretty_term :: NamedTerm -> Doc
pretty_term (NVar ident) = text ident
pretty_term (NAbs ident term) = parens $ text "lambda" <+> text ident <+> pretty_term term
pretty_term (NApp func arg) = parens $ pretty_term func <+> pretty_term arg

render_term :: NamedTerm -> String
render_term = render . pretty_term
