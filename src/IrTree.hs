
-- Intermediate Representation Tree

module IrTree where

import SyntaxTree
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Parser

data IrVarEntry = IrVarEntry { varName :: Ident,
                               sizeInFrame :: Int,
                               fpOffset :: Int }
                deriving (Show)

data IrEnv = IrEnv { varTable :: Map.Map Ident IrVarEntry,
                     parent :: Maybe IrEnv }
           deriving (Show)

type Label = String

data IrStmt = IrMove IrExp IrExp SourcePos
           | IrExpStmt IrExp SourcePos

           -- 1st arg is to be a label or jump address, and 2nd arg is
           -- a list of all possible locations that 1st arg is
           -- evaluated to.
           | IrJump IrExp [IrExp] SourcePos

           -- 2nd and 3rd args are compared with 1st arg (binop), and
           -- jump to the location specified with 4th arg if the
           -- result is true, otherwise jump to 5th arg
           | IrCJump BinaryOp IrExp IrExp Label Label SourcePos

           | IrSeq IrStmt IrStmt SourcePos
           | IrLabel Label SourcePos
             deriving (Show)

data IrExp = IrConstInt Int SourcePos -- constant
           | IrName String SourcePos  -- reference by name
           | IrTemp Int SourcePos -- Temp corresponds to register in assembly
           | IrTempFp SourcePos   -- Frame pointer: special case of Temp
           | IrBinop BinaryOp IrExp IrExp SourcePos
           | IrMem IrExp SourcePos
           | IrCall IrExp [IrExp] SourcePos -- function call
           | IrEseq IrStmt IrExp SourcePos
           deriving (Show)

doTranslateCExp :: IrEnv -> CExp -> IrExp
doTranslateCExp env expr =
    case expr of
      CLitIntExp int pos -> IrConstInt int pos
      CParenExp e _ -> doTranslateCExp env e
      CIdentExp id pos ->
          let ve = (varTable env) Map.! id
          in
            IrMem (IrBinop BinopPlus
                           (IrTempFp pos)
                           (IrConstInt (fpOffset ve) pos)
                           pos)
                  pos
      CFuncallExp id exprs pos ->
          IrCall (IrName id pos) (map (doTranslateCExp env) exprs) pos
      CUnaryExp unop e pos ->   -- TODO: non numeric value handling
          case unop of
            UnopPlus -> doTranslateCExp env e
            UnopMinus -> IrBinop BinopMinus
                                 (IrConstInt 0 pos)
                                 (doTranslateCExp env e)
                                 pos
      CBinaryExp binop e1 e2 pos ->
          IrBinop binop
                  (doTranslateCExp env e1)
                  (doTranslateCExp env e2)
                  pos
      CAssignExp e1 e2 pos ->
          IrEseq (IrMove (doTranslateCExp env e1)
                         (doTranslateCExp env e2)
                         pos)
                 (doTranslateCExp env e1)
                 pos

doTranslateCStmt :: IrEnv -> CStmt -> IrStmt
doTranslateCStmt env stmt =
    case stmt of
      CExpStmt expr pos ->
          IrExpStmt (doTranslateCExp env expr) pos

prettyIrExp :: IrExp -> String
prettyIrExp ie =
    prettyIrExpInner ie 0 [] [False]

prettyIrStmt :: IrStmt -> String
prettyIrStmt is =
    prettyIrStmtInner is 0 [] [False]

prettyIrExpInner ie depth barStack nextBarStack =
    let prefix =
            if depth == 0 then
                ""
            else
                foldl (\s bar -> ' ':((if bar then '|' else ' '):s))
                      " +"
                      (take (depth - 1) barStack)
    in
      case ie of
        IrConstInt int pos -> prefix ++ "CONST(" ++ show int ++ ")\t\t" ++
                              show pos ++ "\n"
        IrName name pos -> prefix ++ "NAME(" ++ name ++ ")\t\t" ++ show pos ++ "\n"
        IrTemp idx pos -> prefix ++ "TEMP(" ++ show idx ++ ")\t\t" ++
                          show pos ++ "\n"
        IrTempFp pos -> prefix ++ "TEMP(FP)\t\t" ++ show pos ++ "\n"
        IrBinop binop ie1 ie2 pos ->
            prefix ++ "BINOP(" ++ binopToString binop ++ ")\t\t" ++
            show pos ++ "\n" ++
            (prettyIrExpInner ie1 (depth + 1) nextBarStack (True : nextBarStack)) ++
            (prettyIrExpInner ie2 (depth + 1) nextBarStack (False : nextBarStack))
        IrMem ie_ pos ->
            prefix ++ "MEM\t\t" ++ show pos ++ "\n" ++
              (prettyIrExpInner ie_ (depth + 1) nextBarStack (False : nextBarStack))
        IrCall ieFunc ieArgs pos ->
            prefix ++ "CALL\t\t" ++ show pos ++ "\n" ++
              (prettyIrExpInner ieFunc (depth + 1) nextBarStack (False : nextBarStack)) ++
              concatMap (\ie_ -> prettyIrExpInner ie_ (depth + 1) nextBarStack (False : nextBarStack)) ieArgs
        IrEseq istm iexp pos ->
            prefix ++ "ESEQ\t\t" ++ show pos ++ "\n" ++
              (prettyIrStmtInner istm (depth + 1) nextBarStack (True : nextBarStack)) ++
              (prettyIrExpInner iexp (depth + 1) nextBarStack (False : nextBarStack))

prettyIrStmtInner is depth barStack nextBarStack =
    let prefix =
            if depth == 0 then
                ""
            else
                foldl (\s bar -> ' ':((if bar then '|' else ' '):s))
                      " +"
                      (take (depth - 1) barStack)
    in
      case is of
        IrMove ie1 ie2 pos ->
            prefix ++ "MOVE\t\t" ++ show pos ++ "\n" ++
              (prettyIrExpInner ie1 (depth + 1) nextBarStack (True : nextBarStack)) ++
              (prettyIrExpInner ie2 (depth + 1) nextBarStack (False : nextBarStack))
        IrExpStmt iexp pos ->
            prefix ++ "EXP\t\t" ++ show pos ++ "\n" ++
              (prettyIrExpInner iexp (depth + 1) nextBarStack (False : nextBarStack))
        IrJump ie labs pos ->
            prefix ++ "JUMP\t\t" ++ show pos ++ "\n" ++
              (prettyIrExpInner ie (depth + 1) nextBarStack (False : nextBarStack)) ++
              concatMap (\ie_ -> prettyIrExpInner ie_ (depth + 1) nextBarStack (False : nextBarStack)) labs
        IrCJump binop ie1 ie2 labT labF pos ->
            prefix ++ "CJUMP(" ++ show (binopToString binop) ++
            ", " ++ labT ++ ", " ++ labF ++ ")\t\t" ++
            show pos ++ "\n" ++
            (prettyIrExpInner ie1 (depth + 1) nextBarStack (True : nextBarStack)) ++
            (prettyIrExpInner ie2 (depth + 1) nextBarStack (False : nextBarStack))
        IrSeq is1 is2 pos ->
            prefix ++ "SEQ\t\t" ++ show pos ++ "\n" ++
              (prettyIrStmtInner is1 (depth + 1) nextBarStack (True : nextBarStack)) ++
              (prettyIrStmtInner is2 (depth + 1) nextBarStack (False : nextBarStack))
        IrLabel lab pos ->
            prefix ++ "LABEL(" ++ lab ++ ")\t\t" ++ show pos ++ "\n"
