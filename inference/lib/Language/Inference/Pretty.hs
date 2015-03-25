module Language.Inference.Pretty (prettyTermType, showTermType, prettyTerm, showTerm) where

import Language.Inference.Semantics
import Language.Inference.Syntax
import Text.PrettyPrint

prettyType :: TyMono -> Doc
prettyType TyBool = text "Bool"
prettyType TyNat  = text "Nat"
prettyType (TyVar var) = text "X" <> text (show var)
prettyType (TyArr x y) = prettyTypeParens x <+> text "->" <+> prettyType y where
    prettyTypeParens :: TyMono -> Doc
    prettyTypeParens x@(TyArr _ _) = parens $ prettyType x
    prettyTypeParens x             = prettyType x

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

prettyTermType :: Term -> TyMono -> Doc
prettyTermType term ty = prettyTerm term <+> text ":" <+> prettyType ty

showTerm :: Term -> String
showTerm = render . prettyTerm

showType :: TyMono -> String
showType = render . prettyType

showTermType :: Term -> TyMono -> String
showTermType tm ty = render $ prettyTermType tm ty
