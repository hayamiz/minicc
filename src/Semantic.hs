
module Semantic where

import SyntaxTree
import Text.ParserCombinators.Parsec
import Parser
import qualified Data.Map as Map

data Block = FuncBlock
           | LoopBlock
           | CompBlock
           | Root
             deriving (Show)

data VarInfo = VarInfo TypeExp SourcePos
             deriving (Show)

data FuncInfo = FuncInfo TypeExp String [TypeExp] (Maybe SourcePos)
              deriving (Show)

type FuncTable = Map.Map String FuncInfo

type VarTable = Map.Map String VarInfo
data Env = Env { block :: Block,
                 varTable :: VarTable,
                 funcTable :: FuncTable,
                 parent :: Maybe Env }
         deriving (Show)

data SemError = InvalidBreakError SourcePos
              | InvalidContinueError SourcePos
              | UndefinedNameError String SourcePos
              | UndefinedFunctionError String SourcePos
              | InvalidLvalError CExp
              | TypeConflictError FuncInfo
              deriving (Show)

builtinFuncTable :: FuncTable
builtinFuncTable =
    foldl (\table (id, funinfo) -> Map.insert id funinfo table)
          Map.empty
          [("print_line", FuncInfo TypInt "print_line" [] Nothing),
           ("print_int", FuncInfo TypInt "print_int" [TypInt] Nothing)]

doSemCheck :: CProgram -> [SemError]
doSemCheck = doSemCheckCProgram

doSemCheckCProgram :: CProgram -> [SemError]
doSemCheckCProgram cprog =
    let (errs, ftable) = extractFuncs cprog
    in
      errs ++
      (concatMap (doSemCheckCDef $ Env {block = Root,
                                        varTable = Map.empty,
                                        parent = Nothing,
                                        funcTable = (Map.union ftable builtinFuncTable)})
                 cprog)

extractFuncs :: CProgram -> ([SemError], FuncTable)
extractFuncs cprog =
    extractFuncsInner cprog [] Map.empty
  where
    extractFuncsInner cprog errs table =
        case cprog of
          [] -> (errs, table)
          (CFuncDef typ id params _ pos) : cdefs ->
              case (Map.lookup id table) of
                Just (finfo @ (FuncInfo typ_ _ paramTyps _))->
                    if typ_ == typ &&
                       all (== True) (map (\ (t, (t_, _)) -> t == t_)
                                          (zip paramTyps params)) then
                        extractFuncsInner cdefs errs table
                    else
                        let errs_ = TypeConflictError finfo : errs
                        in extractFuncsInner cdefs errs_ table
                Nothing ->
                    let finfo = FuncInfo typ id (map fst params) (Just pos)
                        table_ = Map.insert id finfo table
                    in extractFuncsInner cdefs errs table_

-- TODO: check redefinition of 'id'
doSemCheckCDef :: Env -> CDef -> [SemError]
doSemCheckCDef env cdef =
    case cdef of
      CFuncDef typ id params stmt pos ->
          doSemCheckCStmt
              (Env {block = FuncBlock,
                    funcTable = (funcTable env),
                    varTable = foldl (\tbl param -> insertParam param tbl pos)
                                     Map.empty params,
                    parent = Just env})
              stmt

insertParam :: (TypeExp, Ident) -> Map.Map String VarInfo -> SourcePos -> Map.Map String VarInfo
insertParam (typ, id) table pos =
    Map.insert id (VarInfo typ pos) table

doSemCheckCStmt :: Env -> CStmt -> [SemError]
doSemCheckCStmt env stmt =
    case stmt of
      CBreakStmt pos ->
          if (isBreakable env) then
              []
          else
              [InvalidBreakError pos]
      CContinueStmt pos ->
          if (isContinuable env) then
              []
          else
              [InvalidContinueError pos]
      CReturnStmt e pos ->
          let expErrors = doSemCheckCExp env e
          in expErrors          -- TODO: check returning type of exp
      CCompStmt vardecls stmts pos ->
          let vtable_ = foldl (\tbl vdecl -> insertCVarDecl vdecl tbl)
                              Map.empty vardecls
              env_ = Env {block = CompBlock,
                          varTable = vtable_,
                          funcTable = (funcTable env),
                          parent = Just env}
          in
            (concatMap (doSemCheckCVarDecl env_) vardecls) ++
              (concatMap (doSemCheckCStmt env_) stmts)
      CIfStmt e stmt Nothing _ -> -- TODO: check type of predicate exp
          (doSemCheckCExp env e) ++ (doSemCheckCStmt env stmt)
      CIfStmt e stmt (Just elseStmt) _ ->
          (doSemCheckCExp env e) ++ 
              (doSemCheckCStmt env stmt) ++
              (doSemCheckCStmt env elseStmt)
      CWhileStmt e stmt _ -> -- TODO: check type of predicate exp
          let env_ = Env {block = LoopBlock,
                          varTable = Map.empty,
                          funcTable = (funcTable env),
                          parent = Just env}
          in
          (doSemCheckCExp env e) ++ (doSemCheckCStmt env_ stmt)
      CExpStmt e _ ->
          doSemCheckCExp env e
      CNopStmt _ ->
          []

insertCVarDecl :: CVarDecl -> Map.Map String VarInfo -> Map.Map String VarInfo
insertCVarDecl (typ, id, pos) table =
    Map.insert id (VarInfo typ pos) table

isBreakable :: Env -> Bool
isBreakable env =
    case (block env) of
      LoopBlock -> True
      CompBlock ->
          case (parent env) of
            Just p -> isBreakable p
            Nothing -> False
      FuncBlock -> False
      Root -> False

isContinuable = isBreakable

-- TODO:
doSemCheckCExp :: Env -> CExp -> [SemError]
doSemCheckCExp env expr =
    case expr of
      CLitIntExp _ _ -> []
      CParenExp e _ -> doSemCheckCExp env e
      CIdentExp id pos ->
          if isIdentDefined env id then
              []
          else
              [UndefinedNameError id pos]
      CFuncallExp id args pos ->
          let argErrs = (concatMap (doSemCheckCExp env) args)
          in
            case (Map.lookup id (funcTable env)) of
              Nothing -> (UndefinedFunctionError id pos) : argErrs
              Just (FuncInfo typ id paramTyps _) -> -- TODO: arg type check
                  argErrs
      CUnaryExp op e _ -> doSemCheckCExp env e
      CBinaryExp op e1 e2 _ ->
          (doSemCheckCExp env e1) ++ (doSemCheckCExp env e2)
      CAssignExp e1 e2 _ ->     -- TODO: type checking
          (doSemCheckLval env e1) ++ (doSemCheckCExp env e2)

doSemCheckLval :: Env -> CExp -> [SemError]
doSemCheckLval env expr =
    case expr of
      CIdentExp id pos ->
          if (isIdentDefined env id) then
              []
          else
              [UndefinedNameError id pos]
      _ -> [InvalidLvalError expr]

isIdentDefined :: Env -> String -> Bool
isIdentDefined env id =
    case (Map.lookup id (varTable env)) of
      Just varinfo -> True
      Nothing ->
          case (parent env) of
            Just p -> isIdentDefined p id
            Nothing -> False

doSemCheckCVarDecl :: Env -> CVarDecl -> [SemError]
doSemCheckCVarDecl a b = []
