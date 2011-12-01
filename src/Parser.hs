
module Parser where

import Text.ParserCombinators.Parsec
import Data.List

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
program =
    do spaces
       ret <- many funcDef
       return ret

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
             spaces
             return $ CFuncDef typ id params stmt

typeExp :: GenParser Char st TypeExp
typeExp = do try $ string "int"; return TypInt

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
               do try $ string "break"; spaces; char ';'; return CBreakStmt
               <|>
               do try $ string "continue"; spaces; char ';'; return CContinueStmt
               <|>
               do try $ string "return"; spaces; e <- expr; spaces; char ';';
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
ifStmt = do try $ string "if"
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
       spaces
       char ';'
       spaces
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
    do try $ string "=="; return BinopEq
    <|>
    do try $ string "!="; return BinopNeq

eqlExp :: GenParser Char st CExp
eqlExp = 
    do e1 <- relExp
       spaces
       es <- many (do op <- eqlOp
                      spaces
                      e <- relExp
                      spaces
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

relOp :: GenParser Char st BinaryOp
relOp =
    do try $ string "<="; return BinopLe
    <|>
    do try $ string ">="; return BinopGe
    <|>
    do try $ string "<"; return BinopLt
    <|>
    do try $ string ">"; return BinopGt

relExp :: GenParser Char st CExp
relExp = 
    do e1 <- addExp
       spaces
       es <- many (do op <- relOp
                      spaces
                      e <- addExp
                      spaces
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

addOp :: GenParser Char st BinaryOp
addOp =
    do try $ string "+"; return BinopPlus
    <|>
    do try $ string "-"; return BinopMinus

addExp :: GenParser Char st CExp
addExp = 
    do spaces
       e1 <- mulExp
       spaces
       es <- many (do op <- addOp
                      spaces
                      e <- mulExp
                      spaces
                      return (op, e))
       return $ foldl (\e_ (op, e__) -> CBinaryExp op e_ e__) e1 es

mulOp :: GenParser Char st BinaryOp
mulOp =
    do try $ string "*"; return BinopMult
    <|>
    do try $ string "/"; return BinopDiv
    <|>
    do try $ string "%"; return BinopMod

mulExp :: GenParser Char st CExp
mulExp = 
    do e1 <- unaryExp
       spaces
       es <- many (do op <- mulOp
                      spaces
                      e <- unaryExp
                      spaces
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

parseProgram :: FilePath -> String -> CProgram
parseProgram filename str =
    let ret = parse program filename str
    in case ret of
         Right prog -> prog
         Left _ -> []

parseFile :: FilePath -> IO CProgram
parseFile filename =
    do
      str <- readFile filename
      return $ parseProgram filename str

-- printer

cProgramToString :: CProgram -> String
cProgramToString = concat . intersperse "\n" . map cDefToString

cDefToString :: CDef -> String
cDefToString cdef =
    case cdef of
      CFuncDef typ id params body ->
          (typeToString typ)
          ++ " " ++ id ++
          "(" ++ (paramListToString params) ++ ") " ++
          (cStmtToString body) ++ "\n"

typeToString :: TypeExp -> String
typeToString typ = "int"

paramListToString :: [(TypeExp, Ident)] -> String
paramListToString =
    concat . intersperse ", " . map paramToString
    where
      paramToString (typ, id) = (typeToString typ) ++ " " ++ id

cStmtToString :: CStmt -> String
cStmtToString stmt =
    case stmt of
      CBreakStmt -> "break;"
      CContinueStmt -> "continue;"
      CReturnStmt e -> "return " ++ (cExpToString e) ++ ";"
      CCompStmt decls stmts ->
          let declsStr = concat $ map (++ "\n") $ map cVarDeclToString decls
              stmtsStr = concat $ map (++ "\n") $ map cStmtToString stmts
          in
          "{\n" ++ declsStr ++ stmtsStr ++ "}"
      CIfStmt e stmt Nothing ->
          "if (" ++ (cExpToString e) ++ ") " ++ (cStmtToString stmt)
      CIfStmt e stmt (Just elseStmt) ->
          "if (" ++ (cExpToString e) ++ ") " ++ (cStmtToString stmt) ++
          "\n else " ++ (cStmtToString elseStmt)
      CWhileStmt e body ->
          "while(" ++ (cExpToString e) ++ ") " ++ (cStmtToString body)
      CExpStmt e -> (cExpToString e)  ++ ";"
      CNopStmt -> ";"
      _ -> show stmt

cVarDeclToString :: CVarDecl -> String
cVarDeclToString (typ, id) = (typeToString typ) ++ " " ++ id ++ ";"

cExpToString :: CExp -> String
cExpToString expr =
    case expr of
      CLitIntExp int -> show int
      CParenExp e -> "(" ++ (cExpToString e) ++ ")"
      CIdentExp id -> id
      CFuncallExp fname exprs ->
          fname ++ "(" ++ (concat $ intersperse ", " $ map cExpToString exprs) ++ ")"
      CUnaryExp op e ->
          (unopToString op) ++ " " ++ (cExpToString e)
      CBinaryExp op e1 e2 ->
          (cExpToString e1) ++ " " ++ (binopToString op) ++ " " ++ (cExpToString e2)
      CAssignExp e1 e2 ->
          (cExpToString e1) ++ " = " ++ (cExpToString e2)
      _ -> show expr

unopToString :: UnaryOp -> String
unopToString unop =
    case unop of
      UnopPlus -> "+"
      UnopMinus -> "-"
      UnopNot -> "!"

binopToString :: BinaryOp -> String
binopToString binop =
    case binop of
      BinopEq ->	"=="
      BinopNeq ->	"!="
      BinopGt ->	">"
      BinopLt ->	"<"
      BinopGe ->	">="
      BinopLe ->	"<="
      BinopPlus ->	"+"
      BinopMinus ->	"-"
      BinopMult ->	"*"
      BinopDiv ->	"/"
      BinopMod ->	"%"