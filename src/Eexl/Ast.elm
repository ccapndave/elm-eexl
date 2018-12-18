module Eexl.Ast exposing (BinaryOp(..), Expr(..), Literal(..), UnaryOp(..))


type Expr
    = Literal Literal
    | UnaryOp UnaryOp
    | BinaryOp BinaryOp


type Literal
    = Int Int
    | Bool Bool
    | Var String


type UnaryOp
    = Not Expr


type BinaryOp
    = Lt Expr Expr
    | Lte Expr Expr
    | Eq Expr Expr
    | Mt Expr Expr
    | Mte Expr Expr
    | And Expr Expr
    | Or Expr Expr
