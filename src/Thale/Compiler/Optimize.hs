module Thale.Compiler.Optimize (optimize) where

import Thale.Compiler.Ir
  ( IRCmpOp (..),
    IRFunction (..),
    IRInstr (..),
    IRLiteral (..),
    IROp (..),
    IRProgram (..),
  )

optimize :: IRProgram -> IRProgram
optimize (IRProgram funs) = IRProgram (map optimizeFunction funs)

optimizeFunction :: IRFunction -> IRFunction
optimizeFunction f =
  let folded = constantFold (irBody f)
   in f {irBody = folded}

constantFold :: [IRInstr] -> [IRInstr]
constantFold = go [] []
  where
    go _ acc [] = reverse acc
    go env acc (instr : is) =
      case instr of
        IRConst name lit ->
          go ((name, lit) : env) (IRConst name lit : acc) is
        IRBinOp dst op a b ->
          case (lookup a env, lookup b env) of
            (Just la, Just lb)
              | Just res <- evalBinOp op la lb ->
                  go ((dst, res) : env) (IRConst dst res : acc) is
            _ -> go (remove dst env) (IRBinOp dst op (replace env a) (replace env b) : acc) is
        IRCmp dst op a b ->
          case (lookup a env, lookup b env) of
            (Just la, Just lb)
              | Just res <- evalCmp op la lb ->
                  go ((dst, res) : env) (IRConst dst res : acc) is
            _ -> go (remove dst env) (IRCmp dst op (replace env a) (replace env b) : acc) is
        IRCall (Just dst) fn args ->
          go (remove dst env) (IRCall (Just dst) fn (map (replace env) args) : acc) is
        IRCall Nothing fn args ->
          go env (IRCall Nothing fn (map (replace env) args) : acc) is
        IRReturn val ->
          go env (IRReturn (replaceMaybe env val) : acc) is
        _ -> go env (instr : acc) is

    replace env name =
      case lookup name env of
        Just (LFloat v) -> show v
        Just (LBool v) -> show v
        _ -> name
    replaceMaybe env = fmap (replace env)
    remove n = filter ((/= n) . fst)

evalBinOp :: IROp -> IRLiteral -> IRLiteral -> Maybe IRLiteral
evalBinOp op (LFloat a) (LFloat b) = case op of
  IRAdd -> Just (LFloat (a + b))
  IRSub -> Just (LFloat (a - b))
  IRMul -> Just (LFloat (a * b))
  IRDiv -> Just (LFloat (a / b))
evalBinOp _ _ _ = Nothing

evalCmp :: IRCmpOp -> IRLiteral -> IRLiteral -> Maybe IRLiteral
evalCmp op (LFloat a) (LFloat b) = case op of
  IRLt -> Just (LBool (a < b))
  IRGt -> Just (LBool (a > b))
  IREq -> Just (LBool (a == b))
evalCmp op (LBool a) (LBool b) = case op of
  IREq -> Just (LBool (a == b))
  _ -> Nothing
evalCmp _ _ _ = Nothing