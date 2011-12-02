
module Parser where

import Text.ParserCombinators.Parsec
import Data.List
import SyntaxTree

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
       spaces
       eof
       return ret

funcDef :: GenParser Char st CDef
funcDef = do pos <- getPosition
             typ <- typeExp
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
             return $ CFuncDef typ id params stmt pos

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
       pos <- getPosition
       ret <- (do char ';'; return $ CNopStmt pos
               <|>
               do try $ string "break"
                  spaces; char ';'
                  return $ CBreakStmt pos
               <|>
               do try $ string "continue"; spaces; char ';'
                  return $ CContinueStmt pos
               <|>
               do try $ string "return"; spaces; e <- expr; spaces; char ';';
                  return $ CReturnStmt e pos
               <|>
               compStmt
               <|>
               ifStmt
               <|>
               whileStmt
               <|>
               do e <- expr; char ';'; return $ CExpStmt e pos)
       spaces
       return ret

ifStmt :: GenParser Char st CStmt
ifStmt = do pos <- getPosition
            try $ string "if"
            spaces; char '('
            pred <- expr
            char ')'
            stmt1 <- stmt
            stmt2 <- option Nothing
                            (do string "else"
                                stmt_ <- stmt
                                return $ Just stmt_)
            return $ CIfStmt pred stmt1 stmt2 pos

whileStmt :: GenParser Char st CStmt
whileStmt =
    do pos <- getPosition
       try $ string "while"
       spaces
       char '('
       pred <- expr
       char ')'
       stmt_ <- stmt
       return $ CWhileStmt pred stmt_ pos

compStmt :: GenParser Char st CStmt
compStmt = do pos <- getPosition
              char '{'; spaces
              vdecls <- many varDecl
              spaces
              stmts <- many stmt
              spaces; char '}'
              return $ CCompStmt vdecls stmts pos

varDecl :: GenParser Char st CVarDecl
varDecl =
    do pos <- getPosition
       typ <- typeExp
       spaces
       ident <- identifier
       spaces
       char ';'
       spaces
       return (typ, ident, pos)

-- Expressions

expr :: GenParser Char st CExp
expr = 
    do pos <- getPosition
       e1 <- eqlExp
       spaces
       opt <- option Nothing
                     (do char '='
                         spaces
                         e <- expr
                         return (Just e))
       spaces
       return (case opt of
                 Nothing -> e1
                 Just e2 -> CAssignExp e1 e2 pos)

eqlOp :: GenParser Char st BinaryOp
eqlOp =
    do try $ string "=="; return BinopEq
    <|>
    do try $ string "!="; return BinopNeq

genBiopExp :: GenParser Char st BinaryOp -> GenParser Char st CExp -> GenParser Char st CExp
genBiopExp opParser childExpParser =
    do pos <- getPosition
       e1 <- childExpParser
       spaces
       es <- many (do op <- opParser
                      spaces
                      e <- childExpParser
                      spaces
                      return (op, e))
       return $ foldl (\ e_ (op, e__) -> CBinaryExp op e_ e__ pos) e1 es

eqlExp :: GenParser Char st CExp
eqlExp = genBiopExp eqlOp relExp

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
relExp = genBiopExp relOp addExp

addOp :: GenParser Char st BinaryOp
addOp =
    do try $ string "+"; return BinopPlus
    <|>
    do try $ string "-"; return BinopMinus

addExp :: GenParser Char st CExp
addExp = genBiopExp addOp mulExp

mulOp :: GenParser Char st BinaryOp
mulOp =
    do try $ string "*"; return BinopMult
    <|>
    do try $ string "/"; return BinopDiv
    <|>
    do try $ string "%"; return BinopMod

mulExp :: GenParser Char st CExp
mulExp = genBiopExp mulOp unaryExp

unaryOp :: GenParser Char st UnaryOp
unaryOp =
    do char '+'; return UnopPlus
    <|>
    do char '-'; return UnopMinus
    <|>
    do char '!'; return UnopNot

unaryExp :: GenParser Char st CExp
unaryExp =
    do pos <- getPosition
       intlit <- intLiteral
       return $ CLitIntExp (read intlit :: Int) pos
    <|>
    do pos <- getPosition
       ident <- identifier
       spaces
       opt <- option Nothing
                     (do char '('
                         arglist <- argExpList
                         char ')'
                         return (Just arglist))
       return (case opt of
                 Nothing -> CIdentExp ident pos
                 Just arglist -> CFuncallExp ident arglist pos)
    <|>
    do pos <- getPosition
       char '('
       spaces
       e <- expr
       spaces
       char ')'
       return $ CParenExp e pos
    <|>
    do pos <- getPosition
       uop <- unaryOp
       spaces
       uexp <- unaryExp
       return $ CUnaryExp uop uexp pos

argExpList :: GenParser Char st [CExp]
argExpList = sepBy expr (do spaces; char ','; spaces)

--

parseProgram :: FilePath -> String -> CProgram
parseProgram filename str =
    let ret = parse program filename str
    in case ret of
         Right prog -> prog
         Left err -> error $ show err

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
      CFuncDef typ id params body _ ->
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
      CBreakStmt _ -> "break;"
      CContinueStmt _ -> "continue;"
      CReturnStmt e _ -> "return " ++ (cExpToString e) ++ ";"
      CCompStmt decls stmts _ ->
          let declsStr = concat $ map (++ "\n") $ map cVarDeclToString decls
              stmtsStr = concat $ map (++ "\n") $ map cStmtToString stmts
          in
          "{\n" ++ declsStr ++ stmtsStr ++ "}"
      CIfStmt e stmt Nothing _ ->
          "if (" ++ (cExpToString e) ++ ") " ++ (cStmtToString stmt)
      CIfStmt e stmt (Just elseStmt) _ ->
          "if (" ++ (cExpToString e) ++ ") " ++ (cStmtToString stmt) ++
          "\n else " ++ (cStmtToString elseStmt)
      CWhileStmt e body _ ->
          "while(" ++ (cExpToString e) ++ ") " ++ (cStmtToString body)
      CExpStmt e _ -> (cExpToString e)  ++ ";"
      CNopStmt _ -> ";"

cVarDeclToString :: CVarDecl -> String
cVarDeclToString (typ, id, _) = (typeToString typ) ++ " " ++ id ++ ";"

cExpToString :: CExp -> String
cExpToString expr =
    case expr of
      CLitIntExp int _ -> show int
      CParenExp e _ -> "(" ++ (cExpToString e) ++ ")"
      CIdentExp id _ -> id
      CFuncallExp fname exprs _ ->
          fname ++ "(" ++ (concat $ intersperse ", " $ map cExpToString exprs) ++ ")"
      CUnaryExp op e _ ->
          (unopToString op) ++ " " ++ (cExpToString e)
      CBinaryExp op e1 e2 _ ->
          (cExpToString e1) ++ " " ++ (binopToString op) ++ " " ++ (cExpToString e2)
      CAssignExp e1 e2 _ ->
          (cExpToString e1) ++ " = " ++ (cExpToString e2)

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