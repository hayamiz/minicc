
module SyntaxTree where

import Text.ParserCombinators.Parsec

type Filename = String
type Lineno = Int

data UnaryOp = UnopPlus | UnopMinus | UnopNot deriving (Show)
data BinaryOp = BinopEq
              | BinopNeq
              | BinopGt
              | BinopLt
              | BinopGe
              | BinopLe
              | BinopPlus
              | BinopMinus
              | BinopMult
              | BinopDiv
              | BinopMod deriving (Show)
data Keyword = KwBreak
             | KwContinue
             | KwElse
             | KwIf
             | KwInt
             | KwReturn
             | KwWhile
type Ident = String
data TypeExp = TypInt deriving (Show, Eq)

type CProgram = [CDef]

data CStmt = CBreakStmt SourcePos
           | CContinueStmt SourcePos
           | CReturnStmt CExp SourcePos
           | CCompStmt [CVarDecl] [CStmt] SourcePos
           | CIfStmt CExp CStmt (Maybe CStmt) SourcePos
           | CWhileStmt CExp CStmt SourcePos
           | CExpStmt CExp SourcePos
           | CNopStmt SourcePos
             deriving (Show)

type CVarDecl = (TypeExp, Ident, SourcePos)

data CExp = CLitIntExp Int SourcePos
          | CParenExp CExp SourcePos
          | CIdentExp Ident SourcePos
          | CFuncallExp Ident [CExp] SourcePos
          | CUnaryExp UnaryOp CExp SourcePos
          | CBinaryExp BinaryOp CExp CExp SourcePos
          | CAssignExp CExp CExp SourcePos
            deriving (Show)

data CDef = CFuncDef TypeExp Ident [(TypeExp, Ident)] CStmt SourcePos deriving (Show)
