module Statement(T, parse, toString, fromString, execute) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr

type T = Statement
data Statement =
    Assignment String Expr.T 
    | Skip
    | Begin [Statement]
    | If Expr.T Statement Statement
    | While Expr.T Statement
    | Read String
    | Write Expr.T
    deriving Show

assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> uncurry Assignment

skipStmt :: Parser Statement
skipStmt = accept "skip" #- require ";" >-> const Skip -- const to ignore parsed string "skip"

readStmt :: Parser Statement
readStmt = accept "read" -# word #- require ";" >-> Read

writeStmt :: Parser Statement
writeStmt = accept "write" -# Expr.parse #- require ";" >-> Write

beginStmt :: Parser Statement
beginStmt = accept "begin" -# iter statement #- require "end" >-> Begin

ifStmt :: Parser Statement
ifStmt = (((accept "if" -# Expr.parse #- require "then") # statement #- require "else") # statement)
    >-> buildIf

buildIf :: ((Expr.T, Statement), Statement) -> Statement
buildIf ((cond, thenStmt), elseStmt) =
    If cond thenStmt elseStmt

whileStmt :: Parser Statement
whileStmt = 
    ((accept "while" -# Expr.parse #- require "do") # statement)
    >-> buildWhile
buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (cond, body) =
    While cond body

statement :: Parser Statement
statement =
    skipStmt
    ! beginStmt
    ! ifStmt
    ! whileStmt
    ! readStmt
    ! writeStmt
    ! assignment

--helper function: when executing statements, we need to turn that into an Integer, or crash with error if evaluation failed
eval :: Expr.T -> Dictionary.T String Integer -> Integer
eval expr dict = 
        case Expr.value expr dict of
                Left message -> error message
                Right v -> v 

class Executable t where
    execute :: [t] -> Dictionary.T String Integer -> [Integer] -> [Integer]

instance Executable Statement where
    -- execute :: [Statement] -> Dictionary.T String Integer -> [Integer] -> [Integer]

    execute [] _ _ = []

    execute (Assignment var expr : stmts) dict input =
        execute stmts newDict input
        where 
            newValue = eval expr dict
            newDict = Dictionary.insert (var, newValue) dict 

    execute (Skip : stmts) dict input =
        execute stmts dict input
    
    execute (Begin block : stmts) dict input =
        execute (block ++ stmts) dict input

    execute (If cond thenStmts elseStmts: stmts) dict input =
        --case (Expr.value cond dict) of
        --    Left err -> error err
        --    Right v ->
        --        if v > 0 then
        --            execute (thenStmts: stmts) dict input
        --        else
        --            execute (elseStmts: stmts) dict input

        if eval cond dict > 0
            then execute (thenStmts : stmts) dict input
            else execute (elseStmts : stmts) dict input

    execute (While cond body : stmts) dict input =
        if eval cond dict > 0
            then execute (body : While cond body :stmts) dict input
            else execute stmts dict input


    execute (Read _ : _) _ [] =
        error "no input to read"

    execute (Read var :stmts) dict (x:xs) =
        execute stmts newDict xs
        where
            newDict = Dictionary.insert (var, x) dict 
    
    execute (Write expr :stmts) dict input =
        eval expr dict : execute stmts dict input

-- helper function for indentation
indent :: Int -> String
indent n = replicate n ' '

showWithIndent :: Int -> Statement -> String
showWithIndent n (Assignment var expr) =
    indent n ++ var ++ " :=" ++ Expr.toString expr ++ ";\n"
showWithIndent n Skip =
    indent n ++ "skip;\n"
showWithIndent n (Read var) =
    indent n ++ "read " ++ var ++ ";\n"
showWithIndent n (Write expr) =
    indent n ++ "write " ++ Expr.toString expr ++ ";\n"
showWithIndent n (Begin stmts) =
    indent n ++ "begin\n" ++
    concatMap (showWithIndent (n + 4)) stmts ++
    indent n ++ "end\n"
showWithIndent n (If cond thenStmt elseStmt) =
    indent n ++ "if " ++ Expr.toString cond ++ " then\n" ++
    showWithIndent (n + 4) thenStmt ++
    indent n ++ "else\n" ++
    showWithIndent (n + 4) elseStmt
showWithIndent n (While cond body) =
    indent n ++ "while " ++ Expr.toString cond ++ " do\n" ++
    showWithIndent (n + 4) body 


instance Parse Statement where
  --parse = error "Statement.parse not implemented"
  parse = statement
  --toString = error "Statement.toString not implemented"
  toString = showWithIndent 0