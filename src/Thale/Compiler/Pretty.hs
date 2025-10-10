module Thale.Compiler.Pretty (markRecursive, prettyExpr) where

import Data.List (intercalate)
import Thale.Compiler.Ast (Expr (..), Type (..))
import Prelude

prettyType :: Type -> String
prettyType TypeFloat = "Float"
prettyType TypeChar = "Char"
prettyType TypeUnit = "Unit"
prettyType TypeBool = "Bool"
prettyType TypeInfer = "Infer"
prettyType (TypeList t) = "List[" ++ prettyType t ++ "]"
prettyType (TypeTuple ts) = "(" ++ joinTypes ts ++ ")"
  where
    joinTypes :: [Type] -> String
    joinTypes [] = ""
    joinTypes [x] = prettyType x
    joinTypes (x : xs) = prettyType x ++ ", " ++ joinTypes xs

markRecursive :: Expr -> Expr
markRecursive = markRec Nothing

markRec :: Maybe String -> Expr -> Expr
markRec ctx (Seq exprs) = Seq (map (markRec ctx) exprs)
markRec ctx (FunDecl name params ret body _) =
  let body' = markRec (Just name) body
      isRec = Just name == ctx || callsSelfInContext name body'
   in FunDecl name params ret body' isRec
markRec ctx (Call fn args) = Call (markRec ctx fn) (map (markRec ctx) args)
markRec ctx (Assign x e) = Assign x (markRec ctx e)
markRec ctx (ValDecl x t e) = ValDecl x t (markRec ctx e)
markRec ctx (PropertyAccess obj prop) = PropertyAccess (markRec ctx obj) prop
markRec ctx (Match e branches) =
  Match (markRec ctx e) [(markRec ctx pat, markRec ctx res) | (pat, res) <- branches]
markRec ctx (Add l r) = Add (markRec ctx l) (markRec ctx r)
markRec ctx (Sub l r) = Sub (markRec ctx l) (markRec ctx r)
markRec ctx (Mul l r) = Mul (markRec ctx l) (markRec ctx r)
markRec ctx (Div l r) = Div (markRec ctx l) (markRec ctx r)
markRec ctx (Mod l r) = Mod (markRec ctx l) (markRec ctx r)
markRec ctx (Pow l r) = Pow (markRec ctx l) (markRec ctx r)
markRec ctx (Less l r) = Less (markRec ctx l) (markRec ctx r)
markRec ctx (Greater l r) = Greater (markRec ctx l) (markRec ctx r)
markRec ctx (Not e) = Not (markRec ctx e)
markRec _ e = e

callsSelfInContext :: String -> Expr -> Bool
callsSelfInContext name (Call (Var f) _) = f == name
callsSelfInContext name (Call fn args) = callsSelfInContext name fn || any (callsSelfInContext name) args
callsSelfInContext name (Seq exprs) = any (callsSelfInContext name) exprs
callsSelfInContext name (FunDecl _ _ _ body _) = callsSelfInContext name body
callsSelfInContext name (Match e branches) =
  callsSelfInContext name e || any (\(pat, res) -> callsSelfInContext name pat || callsSelfInContext name res) branches
callsSelfInContext _ _ = False

joinLines :: [String] -> String
joinLines = intercalate "\n"

prettyExpr :: Expr -> String
prettyExpr = go 0
  where
    indent n = replicate (n * 2) ' '

    go n (FloatLit f) = indent n ++ "FloatLiteral " ++ show f
    go n (CharLit c) = indent n ++ "CharLiteral '" ++ [c] ++ "'"
    go n (BoolLit b) = indent n ++ "BoolLiteral " ++ show b
    go n (Var x) = indent n ++ "Variable " ++ x
    go n (Add l r) = indent n ++ "Add\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Sub l r) = indent n ++ "Sub\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Mul l r) = indent n ++ "Mul\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Div l r) = indent n ++ "Div\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Mod l r) = indent n ++ "Mod\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Pow l r) = indent n ++ "Pow\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Less l r) = indent n ++ "Less\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Greater l r) = indent n ++ "Greater\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Not e) = indent n ++ "Not\n" ++ go (n + 1) e
    go n (NotEqual l r) = indent n ++ "NotEqual\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (LogicalAnd l r) = indent n ++ "LogicalAnd\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (LogicalOr l r) = indent n ++ "LogicalOr\n" ++ go (n + 1) l ++ "\n" ++ go (n + 1) r
    go n (Assign x e) = indent n ++ "Assign " ++ x ++ "\n" ++ go (n + 1) e
    go n Wildcard = indent n ++ "Wildcard"
    go n (ValDecl x t e) =
      indent n
        ++ "ValDecl "
        ++ x
        ++ " : "
        ++ prettyType t
        ++ "\n"
        ++ go (n + 1) e
    go n (ValDeclTuple names t e) =
      indent n
        ++ "ValDeclTuple ("
        ++ concatMap (++ ", ") (init names)
        ++ last names
        ++ ") : "
        ++ prettyType t
        ++ "\n"
        ++ go (n + 1) e
    go n (FunDecl name params ret body isRec) =
      indent n
        ++ "FunDecl "
        ++ name
        ++ "("
        ++ paramList params
        ++ ")"
        ++ " -> "
        ++ prettyType ret
        ++ (if isRec then " [recursive]" else "")
        ++ "\n"
        ++ go (n + 1) body
      where
        paramList [] = ""
        paramList ps = intercalate ", " [x ++ " : " ++ prettyType t | (x, t) <- ps]
    go n (Call fn args) =
      indent n
        ++ "Call\n"
        ++ go (n + 1) fn
        ++ (if null args then "" else "\n" ++ joinLines (map (go (n + 1)) args))
    go n (PropertyAccess obj prop) =
      indent n ++ "PropertyAccess\n" ++ go (n + 1) obj ++ "\n" ++ indent (n + 1) ++ "Property " ++ prop
    go n (Match e branches) =
      indent n ++ "Match\n" ++ go (n + 1) e ++ "\n" ++ joinLines (map (printBranch (n + 1)) branches)
      where
        printBranch lvl (pat, res) =
          indent lvl
            ++ "Branch\n"
            ++ indent (lvl + 1)
            ++ "Pattern:\n"
            ++ go (lvl + 2) pat
            ++ "\n"
            ++ indent (lvl + 1)
            ++ "Result:\n"
            ++ go (lvl + 2) res
    go n (ListLit es) =
      indent n
        ++ "ListLit"
        ++ (if null es then "" else "\n" ++ joinLines (map (go (n + 1)) es))
    go n (Seq es) =
      indent n ++ "Seq\n" ++ joinLines (map (go (n + 1)) es)
    go n (UseExpr moduleName) = indent n ++ "Use " ++ moduleName
    go n (Tuple es) =
      indent n ++ "Tuple\n" ++ joinLines (map (go (n + 1)) es)