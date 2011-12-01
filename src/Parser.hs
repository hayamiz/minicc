
import Text.ParserCombinators.Parsec

-- Syntax Tree

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

-- Tokens

nonDigit :: GenParser Char st Char
nonDigit = oneOf (['a'..'z'] ++ ['A'..'Z'] ++ "_")

identifier :: GenParser Char st String
identifier = do first <- nonDigit
                next <- (many (nonDigit <|> digit))
                return (first : next)

keyword :: GenParser Char at Keyword
keyword =
    do string "break"; return KwBreak
    <|>
    do string "continue"; return KwContinue
    <|>
    do string "else"; return KwElse
    <|>
    do string "if"; return KwIf
    <|>
    do string "int"; return KwInt
    <|>
    do string "return"; return KwReturn
    <|>
    do string "while"; return KwWhile

intLiteral :: GenParser Char st String
intLiteral = many1 digit


-- Grammars

program :: GenParser Char st CProgram
program = many funcDef

funcDef :: GenParser Char st CDef
funcDef = do typ <- typeExp
             spaces
             id <- identifier
             spaces
             char '('
             spaces
             params <- paramList
             spaces
             char ')'
             spaces
             stmt <- compStmt
             return $ CFuncDef typ id params stmt

typeExp :: GenParser Char st TypeExp
typeExp = do string "int"; return TypInt

param :: GenParser Char st (TypeExp, String)
param = do typ <- typeExp
           spaces
           ident <- identifier
           return (typ, ident)

paramList :: GenParser Char st [(TypeExp, Ident)]
paramList = sepBy param (do spaces; char ','; spaces)

-- Statements

stmt :: GenParser Char st CStmt
stmt =
    do spaces
       ret <- (do char ';'; return CNopStmt
               <|>
               do string "break"; spaces; char ';'; return CBreakStmt
               <|>
               do string "continue"; spaces; char ';'; return CContinueStmt
               <|>
               do string "return"; spaces; e <- expr; spaces; char ';';
                  return $ CReturnStmt e
               <|>
               compStmt
               <|>
               ifStmt
               <|>
               whileStmt
               <|>
               do e <- expr; char ';'; return $ CExpStmt e)
       spaces
       return ret

ifStmt :: GenParser Char st CStmt
ifStmt = do string "if"
            spaces; char '('
            pred <- expr
            char ')'
            stmt1 <- stmt
            stmt2 <- option Nothing
                            (do string "else"
                                stmt_ <- stmt
                                return $ Just stmt_)
            return $ CIfStmt pred stmt1 stmt2

whileStmt :: GenParser Char st CStmt
whileStmt =
    do string "while"
       spaces
       char '('
       pred <- expr
       char ')'
       stmt_ <- stmt
       return $ CWhileStmt pred stmt_

compStmt :: GenParser Char st CStmt
compStmt = do char '{'; spaces
              vdecls <- many varDecl
              spaces
              stmts <- many stmt
              spaces; char '}'
              return $ CCompStmt vdecls stmts

varDecl :: GenParser Char st CVarDecl
varDecl =
    do typ <- typeExp
       spaces
       ident <- identifier
       return (typ, ident)

-- Expressions

expr :: GenParser Char st CExp
expr = 
    do e1 <- eqlExp
       spaces
       opt <- option Nothing
                     (do char '='
                         spaces
                         e <- expr
                         return (Just e))
       spaces
       return (case opt of
                 Nothing -> e1
                 Just e2 -> CAssignExp e1 e2)

eqlOp :: GenParser Char st BinaryOp
eqlOp =
    do string "=="; return BinopEq
    <|>
    do string "!="; return BinopNeq

eqlExp :: GenParser Char st CExp
eqlExp = 
    do e1 <- relExp
       spaces
       es <- many (do op <- eqlOp
                      spaces
                      e <- relExp
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

relOp :: GenParser Char st BinaryOp
relOp =
    do string "<"; return BinopLt
    <|>
    do string ">"; return BinopGt
    <|>
    do string "<="; return BinopLe
    <|>
    do string ">="; return BinopGe

relExp :: GenParser Char st CExp
relExp = 
    do e1 <- addExp
       spaces
       es <- many (do op <- relOp
                      spaces
                      e <- addExp
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

addOp :: GenParser Char st BinaryOp
addOp =
    do string "+"; return BinopPlus
    <|>
    do string "-"; return BinopMinus

addExp :: GenParser Char st CExp
addExp = 
    do e1 <- mulExp
       spaces
       es <- many (do op <- addOp
                      spaces
                      e <- mulExp
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

mulOp :: GenParser Char st BinaryOp
mulOp =
    do string "*"; return BinopMult
    <|>
    do string "/"; return BinopDiv
    <|>
    do string "%"; return BinopMod

mulExp :: GenParser Char st CExp
mulExp = 
    do e1 <- unaryExp
       spaces
       es <- many (do op <- mulOp
                      spaces
                      e <- unaryExp
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

unaryOp :: GenParser Char st UnaryOp
unaryOp =
    do char '+'; return UnopPlus
    <|>
    do char '-'; return UnopMinus
    <|>
    do char '!'; return UnopNot

unaryExp :: GenParser Char st CExp
unaryExp =
    do intlit <- intLiteral
       return $ CLitIntExp (read intlit :: Int)
    <|>
    do ident <- identifier
       spaces
       opt <- option Nothing
                     (do char '('
                         arglist <- argExpList
                         char ')'
                         return (Just arglist))
       return (case opt of
                 Nothing -> CIdentExp ident
                 Just arglist -> CFuncallExp ident arglist)
    <|>
    do char '('
       spaces
       e <- expr
       spaces
       char ')'
       return $ CParenExp e
    <|>
    do uop <- unaryOp
       spaces
       uexp <- unaryExp
       return $ CUnaryExp uop uexp

argExpList :: GenParser Char st [CExp]
argExpList = sepBy expr (do spaces; char ','; spaces)

--

parseFile filepath = do
  src <- readFile filepath
  print $ parse program filepath src
  return ()