module Language.SimplyTyped.Pretty (prettyTerm, showTerm) where

import Language.SimplyTyped.Syntax
import Text.PrettyPrint

type PTerm = Term String String

prettyTerm :: PTerm -> Doc
prettyTerm TmTrue = text "true"
prettyTerm TmFalse = text "false"
prettyTerm (TmIf x y z) = text "if" <+> prettyTerm x <+> text "then" <+> prettyTerm y <+> text "else" <+> prettyTerm z
prettyTerm (TmVar x) = text x
prettyTerm (TmAbs x t b) = text "\\" <> text x <+> text ":" <+> prettyType t <> text "." <+> prettyTerm b
prettyTerm (TmApp x y) = prettyTermParens x <+> prettyTermParens y

prettyTermParens :: PTerm -> Doc
prettyTermParens x@(TmApp _ _)   = parens $ prettyTerm x
prettyTermParens x@(TmAbs _ _ _) = parens $ prettyTerm x
prettyTermParens x@(TmIf _ _ _)  = parens $ prettyTerm x
prettyTermParens x               = prettyTerm x

prettyType :: Type -> Doc
prettyType TyBool = text "Bool"
prettyType (TyArr x y) = prettyTypeParens x <+> text "->" <+> prettyType y

prettyTypeParens :: Type -> Doc
prettyTypeParens x@(TyArr _ _) = parens $ prettyType x
prettyTypeParens x = prettyType x

showTerm :: PTerm -> String
showTerm = render . prettyTerm
