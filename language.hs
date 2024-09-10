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

data Op = Add | Sub | Mul | Div |GEq

data Expr = Value Val | BinExpr Expr Op Expr | IfElse Expr Expr Expr | Func Var Expr 
    | App Expr Expr

data Val =  ValI Int | ValB Bool 

e1 :: Expr -- 2 + 3 * 5
e1 = BinExpr (Value (ValI 2)) Add (BinExpr (Value (ValI 3))) Mul (BinExpr (Value (ValI 5)))

e2 :: Expr -- if (2>=3) then 5 else false
e2 = IfElse BinExpr (Value (ValI 2)) GEq BinExpr (Value (ValI 3)) (Value (ValI 5)) (Value (ValB False)) 

evaluateOp :: Int -> Op -> Int -> Val
evaluateOp i1 Add i2 = ValI (i1 + i2)
evaluateOp i1 Sub i2 = ValI (i1 - i2)
evaluateOp i1 Mul i2 = ValI (i1 * i2)
evaluateOp i1 Div i2 = ValI (i1 `div` i2)
evaluateOp i1 GEq i2 = ValB (i1 >= i2)


evaluate :: Expr -> Val
evaluate (Value v) = v
evaluate (IfElse c e1 e2) = case evaluate of 
                                ValB True -> evaluate e1
                                ValB False -> evaluate e2
                                ValI 0 -> evaluate e2
                                ValI _ -> evaluate e1
                                --ValI i -> error "condition should be a boolean expression"
evaluate (BinExpr e1 op e2) = case (evaluate e1, evaluate e2) of 
                                (ValI i1, ValI i2) -> evaluateOp i1 op i2
                                _ -> error "operands should be integer"
evaluate _ = error "undefined"


precedence :: Op -> Int
precedence Add = 0
precedence Sub = 0
precedence Mul = 1
precedence Div = 1