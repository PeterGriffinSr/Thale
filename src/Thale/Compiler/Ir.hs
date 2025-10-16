module Thale.Compiler.Ir
  ( IRProgram (..),
    IRFunction (..),
    IRInstr (..),
    IRLiteral (..),
    IROp (..),
    IRCmpOp (..),
    lowerProgram,
  )
where

import Control.Monad (forM_, when)
import Control.Monad.State
  ( State,
    gets,
    modify,
    runState,
  )
import Thale.Compiler.Ast (Expr (..))

newtype IRProgram = IRProgram [IRFunction]
  deriving (Eq, Show)

data IRFunction = IRFunction
  { irName :: String,
    irParams :: [String],
    irBody :: [IRInstr]
  }
  deriving (Eq, Show)

data IRInstr
  = IRLabel String
  | IRConst String IRLiteral
  | IRMove String String
  | IRBinOp String IROp String String
  | IRCmp String IRCmpOp String String
  | IRCall (Maybe String) String [String]
  | IRTailCall String [String]
  | IRTuple String [String]
  | IRProj String String Int
  | IRJump String
  | IRCondJump String String String
  | IRReturn (Maybe String)
  | IRList String [String]
  deriving (Eq, Show)

data IRLiteral
  = LFloat Double
  | LBool Bool
  | LChar Char
  | LList [IRLiteral]
  deriving (Eq, Ord, Show)

data IROp = IRAdd | IRSub | IRMul | IRDiv
  deriving (Eq, Show)

data IRCmpOp = IRLt | IRGt | IREq
  deriving (Eq, Show)

data LowerState = LowerState
  { counter :: Int,
    instrs :: [IRInstr],
    currentFun :: Maybe String,
    constPool :: [(IRLiteral, String)]
  }
  deriving (Eq, Show)

type LowerM = State LowerState

fresh :: LowerM String
fresh = do
  n <- gets counter
  modify $ \s -> s {counter = n + 1}
  pure $ "t" ++ show n

freshLabel :: String -> LowerM String
freshLabel base = do
  n <- fresh
  pure $ base ++ "_" ++ n

emit :: IRInstr -> LowerM ()
emit i = modify $ \s -> s {instrs = instrs s ++ [i]}

withCurrentFun :: String -> LowerM a -> LowerM a
withCurrentFun name action = do
  old <- gets currentFun
  modify $ \s -> s {currentFun = Just name}
  r <- action
  modify $ \s -> s {currentFun = old, constPool = []}
  pure r

runLower :: LowerM a -> (a, [IRInstr])
runLower m =
  let initS = LowerState 0 [] Nothing []
      (a, s') = runState m initS
   in (a, instrs s')

lowerProgram :: Expr -> IRProgram
lowerProgram expr =
  let funs = collectTopLevelFunctions expr
      irfuncs = map lowerFunction funs
   in IRProgram irfuncs

collectTopLevelFunctions :: Expr -> [Expr]
collectTopLevelFunctions (Seq xs) = concatMap collectTopLevelFunctions xs
collectTopLevelFunctions f@(FunDecl _ _ _ body _) =
  f : collectTopLevelFunctions body
collectTopLevelFunctions _ = []

lowerFunction :: Expr -> IRFunction
lowerFunction (FunDecl name params _ body _rec) =
  let paramNames = map fst params
      (_, bodyInstrs) = runLower $ withCurrentFun name $ do
        result <- lowerStmtTail body
        case result of
          Nothing -> emit $ IRReturn Nothing
          Just temp -> emit $ IRReturn (Just temp)
   in IRFunction name paramNames bodyInstrs
lowerFunction _ = error "lowerFunction: expected FunDecl"

getConstTemp :: IRLiteral -> LowerM String
getConstTemp lit = do
  pool <- gets constPool
  case lookup lit pool of
    Just t -> pure t
    Nothing -> do
      t <- fresh
      emit $ IRConst t lit
      modify $ \s -> s {constPool = (lit, t) : constPool s}
      pure t

lowerExpr :: Bool -> Expr -> LowerM String
lowerExpr _ (FloatLit d) = getConstTemp (LFloat d)
lowerExpr _ (BoolLit b) = getConstTemp (LBool b)
lowerExpr _ (CharLit c) = getConstTemp (LChar c)
lowerExpr _ (Var v) = pure v
lowerExpr tailp (Add a b) = binOp tailp IRAdd a b
lowerExpr tailp (Sub a b) = binOp tailp IRSub a b
lowerExpr tailp (Mul a b) = binOp tailp IRMul a b
lowerExpr tailp (Div a b) = binOp tailp IRDiv a b
lowerExpr tailp (Less a b) = cmpOp tailp IRLt a b
lowerExpr tailp (Greater a b) = cmpOp tailp IRGt a b
lowerExpr _ (NotEqual a b) = do
  ta <- lowerExpr False a
  tb <- lowerExpr False b
  tEq <- fresh
  emit $ IRCmp tEq IREq ta tb
  constFalse <- getConstTemp (LBool False)
  tNe <- fresh
  emit $ IRCmp tNe IREq tEq constFalse
  pure tNe
lowerExpr _ (Tuple elems) = do
  ts <- mapM (lowerExpr False) elems
  t <- fresh
  emit $ IRTuple t ts
  pure t
lowerExpr _ (PropertyAccess obj _) = do
  to <- lowerExpr False obj
  t <- fresh
  -- TODO: Replace with actual property index lookup
  let idx = 0
  emit $ IRProj t to idx
  pure t
lowerExpr tailp (Call fnExpr args) = do
  calleeName <- case fnExpr of
    Var v -> pure v
    PropertyAccess o nm -> case o of
      Var v -> pure $ v ++ "." ++ nm
      _ -> do
        otemp <- lowerExpr False o
        pure $ otemp ++ "." ++ nm
    _ -> lowerExpr False fnExpr
  argTemps <- mapM (lowerExpr False) args
  mcur <- gets currentFun
  case (tailp, mcur) of
    (True, Just cur) | cur == calleeName -> do
      emit $ IRTailCall calleeName argTemps
      pure "__tailcall__"
    _ -> do
      dest <- fresh
      emit $ IRCall (Just dest) calleeName argTemps
      pure dest
lowerExpr _ (Assign name rhs) = do
  src <- lowerExpr False rhs
  emit $ IRMove name src
  pure name
lowerExpr _ (ValDecl name _t rhs) = do
  src <- lowerExpr False rhs
  emit $ IRMove name src
  pure name
lowerExpr _ (ValDeclTuple names _tys rhs) = do
  src <- lowerExpr False rhs
  forM_ (zip names [0 ..]) $ \(n, i) -> emit $ IRProj n src i
  pure $ head names
lowerExpr tailp (Seq xs) = do
  case xs of
    [] -> pure "__unit__"
    _ -> do
      let initExprs = init xs
          lastExpr = last xs
      mapM_ (lowerExpr False) initExprs
      lowerExpr tailp lastExpr
lowerExpr tailp (Match scrutinee arms) = lowerMatch tailp scrutinee arms
lowerExpr _ (UseExpr _) = do
  t <- fresh
  emit $ IRConst t (LBool True)
  pure t
lowerExpr _ (ListLit elems) = do
  let allLits = all isLiteral elems
  if allLits
    then do
      let lits = map toLiteral elems
      t <- fresh
      emit $ IRConst t (LList lits)
      pure t
    else do
      ts <- mapM (lowerExpr False) elems
      t <- fresh
      emit $ IRList t ts
      pure t
  where
    isLiteral (FloatLit _) = True
    isLiteral (BoolLit _) = True
    isLiteral (CharLit _) = True
    isLiteral _ = False

    toLiteral (FloatLit d) = LFloat d
    toLiteral (BoolLit b) = LBool b
    toLiteral (CharLit c) = LChar c
    toLiteral _ = error "Non-literal element in toLiteral"

-- IMPORTANT: do not emit a constant when encountering a nested function declaration.
-- Return the function's declared name so calls inside the current function can refer to it.
lowerExpr _ (FunDecl name _params _ret _body _rec) = pure name
lowerExpr _ other = error $ "lowerExpr: unhandled expr: " ++ show other

lowerMatch :: Bool -> Expr -> [(Expr, Expr)] -> LowerM String
lowerMatch tailp scrutinee arms = do
  s <- lowerExpr False scrutinee
  endLabel <- freshLabel "match_end"
  resultTemp <- fresh

  let armToPat (pat, body) = case pat of
        BoolLit b -> Left (LBool b, body)
        FloatLit f -> Left (LFloat f, body)
        Wildcard -> Right body
        _ -> error $ "Match: unsupported pattern in lowering: " ++ show pat

  let (valArms, defaults) =
        foldr
          ( \arm (vs, ds) ->
              case armToPat arm of
                Left (lit, body) -> ((lit, body) : vs, ds)
                Right body -> (vs, body : ds)
          )
          ([], [])
          arms

  let recMatch [] [] = error "Match lowering: empty arms"
      recMatch ((lit, body) : rest) defs = do
        labelNext <- freshLabel "match_arm"
        tmpCmp <- fresh
        tmpLit <- getConstTemp lit
        emit $ IRCmp tmpCmp IREq s tmpLit
        let continueLabel = if null rest && null defs then endLabel else "match_continue_" ++ tmpCmp
        emit $ IRCondJump tmpCmp labelNext continueLabel
        emit $ IRLabel labelNext
        bodyVal <- lowerExpr tailp body
        when (bodyVal /= "__tailcall__") $ emit $ IRMove resultTemp bodyVal
        emit $ IRJump endLabel
        when (not (null rest) || not (null defs)) $ do
          emit $ IRLabel continueLabel
          recMatch rest defs
      recMatch [] (d : _) = do
        bodyVal <- lowerExpr tailp d
        when (bodyVal /= "__tailcall__") $ emit $ IRMove resultTemp bodyVal

  recMatch valArms defaults
  emit $ IRLabel endLabel
  pure resultTemp

binOp :: Bool -> IROp -> Expr -> Expr -> LowerM String
binOp _ op a b = do
  ta <- lowerExpr False a
  tb <- lowerExpr False b
  t <- fresh
  emit $ IRBinOp t op ta tb
  pure t

cmpOp :: Bool -> IRCmpOp -> Expr -> Expr -> LowerM String
cmpOp _ op a b = do
  ta <- lowerExpr False a
  tb <- lowerExpr False b
  t <- fresh
  emit $ IRCmp t op ta tb
  pure t

lowerStmtTail :: Expr -> LowerM (Maybe String)
lowerStmtTail (Seq xs) = case xs of
  [] -> pure Nothing
  _ -> do
    let initExprs = init xs
        lastExpr = last xs
    mapM_ (lowerExpr False) initExprs
    t <- lowerExpr True lastExpr
    if t == "__tailcall__" then pure Nothing else pure (Just t)
lowerStmtTail expr@(Call _ _) = do
  t <- lowerExpr True expr
  if t == "__tailcall__" then pure Nothing else pure (Just t)
lowerStmtTail (ValDecl name _t rhs) = do
  r <- lowerExpr False rhs
  emit $ IRMove name r
  pure (Just r)
lowerStmtTail (ValDeclTuple names _tys rhs) = do
  r <- lowerExpr False rhs
  forM_ (zip names [0 ..]) $ \(n, i) -> emit $ IRProj n r i
  pure (Just $ head names)
lowerStmtTail (Match scr arms) = do
  t <- lowerMatch True scr arms
  pure (Just t)
lowerStmtTail (Assign name rhs) = do
  r <- lowerExpr False rhs
  emit $ IRMove name r
  pure (Just r)
lowerStmtTail other = do
  t <- lowerExpr True other
  if t == "__tailcall__" then pure Nothing else pure (Just t)
