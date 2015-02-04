module ArithPrettyPrint (render_term) where

import Arith
import Text.PrettyPrint

pretty_term :: Term -> Doc
pretty_term Tru = text "#t"
pretty_term Fls = text "#f"
pretty_term Zero = text "#z"
pretty_term (Succ x) = pr_func_app "succ" [x]
pretty_term (Pred x) = pr_func_app "pred" [x]
pretty_term (IsZero x) = pr_func_app "zero?" [x]
pretty_term (If x y z) = pr_func_app "if" [x, y, z]

pr_func_app :: String -> [Term] -> Doc
pr_func_app name = parens . hsep . (:) (text name) . map pretty_term

render_term :: Term -> String
render_term = render . pretty_term
