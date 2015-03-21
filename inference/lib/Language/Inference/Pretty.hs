module Language.Inference.Pretty (prettyTerm, showTerm) where

import Language.Inference.Syntax
import Text.PrettyPrint

prettyTerm :: Term -> Doc
prettyTerm TmTrue = text "true"
prettyTerm TmFalse = text "false"
prettyTerm (TmIf x y z) = text "if" <+> prettyTerm x <+> text "then" <+> prettyTerm y <+> text "else" <+> prettyTerm z
prettyTerm TmZero       = text "0"
prettyTerm (TmSucc x)   = text "succ" <+> prettyTermParens x
prettyTerm (TmRec x y)   = text "rec" <+> prettyTermParens x <+> prettyTermParens y
prettyTerm (TmVar x) = text x
prettyTerm (TmAbs x b) = text "\\" <> text x <+> text "." <+> prettyTerm b
prettyTerm (TmApp x y) = prettyTermParens x <+> prettyTermParens y

prettyTermParens :: Term -> Doc
prettyTermParens x@(TmApp _ _)  = parens $ prettyTerm x
prettyTermParens x@(TmAbs _ _)  = parens $ prettyTerm x
prettyTermParens x@(TmIf _ _ _) = parens $ prettyTerm x
prettyTermParens x@(TmSucc _)   = parens $ prettyTerm x
prettyTermParens x@(TmRec _ _)  = parens $ prettyTerm x
prettyTermParens x              = prettyTerm x

showTerm :: Term -> String
showTerm = render . prettyTerm
