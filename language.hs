-- single line comment

{-
    multi line comment
-}
-- grammar of the language
{-
<program> -> begin <stmts> end
<stmts> -> <stmt> | <stmt>;<stmts>
<stmt> -> <var> = <expr>
<var> -> string
<op> -> + | - | * | /
<expr> -> <val> | <expr> <op> <expr> | if <expr> then <expr> else <expr>  
    | func <var> <expr> | <expr> <expr>
<val> -> integers | booleans
-}

-- abstract data types

data Program = BeginEnd Statements

data Statements = End Stmt | Seq Stmt Statements

data Stmt = Assign Var expr

type Var = string

data Op = Add | Sub | Mult | Div

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Expr 
    | App Expr Expr

data Val =  ValI Int | ValB Bool 