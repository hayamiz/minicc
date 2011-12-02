
module SyntaxTree where

import Text.ParserCombinators.Parsec

type Filename = String
type Lineno = Int
type Location = (Filename, Int)

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
data TypeExp = TypInt deriving (Show)

type CProgram = [CDef]

data CStmt = CBreakStmt
           | CContinueStmt
           | CReturnStmt CExp
           | CCompStmt [CVarDecl] [CStmt]
           | CIfStmt CExp CStmt (Maybe CStmt)
           | CWhileStmt CExp CStmt
           | CExpStmt CExp
           | CNopStmt
             deriving (Show)

type CVarDecl = (TypeExp, Ident)

data CExp = CLitIntExp Int
          | CParenExp CExp
          | CIdentExp Ident
          | CFuncallExp Ident [CExp]
          | CUnaryExp UnaryOp CExp
          | CBinaryExp BinaryOp CExp CExp
          | CAssignExp CExp CExp
            deriving (Show)

data CDef = CFuncDef TypeExp Ident [(TypeExp, Ident)] CStmt deriving (Show)
