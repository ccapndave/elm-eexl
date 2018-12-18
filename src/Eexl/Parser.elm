module Eexl.Parser exposing (parse)

import Eexl.Ast exposing (..)
import Parser exposing (..)
import Set


parse : String -> Result (List DeadEnd) Expr
parse str =
    run expr str


expr : Parser Expr
expr =
    term


term : Parser Expr
term =
    oneOf
        [ succeed identity
            |. symbol "("
            |. spaces
            |= lazy (\_ -> expr)
            |. spaces
            |. symbol ")"
        , binaryOp |> map BinaryOp
        , unaryOp |> map UnaryOp
        , literal |> map Literal
        ]


int : Parser Int
int =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= Parser.int
        , Parser.int
        ]


literal : Parser Literal
literal =
    oneOf
        [ int |> map Int
        , keyword "true" |> map (\_ -> Bool True)
        , keyword "false" |> map (\_ -> Bool False)
        , variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
            |> map Var
        ]


unaryOp : Parser UnaryOp
unaryOp =
    oneOf
        [ succeed Not
            |. symbol "!"
            |= lazy (\_ -> expr)
        ]


binaryOp : Parser BinaryOp
binaryOp =
    let
        makeBinaryParser : String -> (Expr -> Expr -> BinaryOp) -> Parser BinaryOp
        makeBinaryParser binarySymbol tagger =
            succeed tagger
                |= lazy (\_ -> expr)
                |. spaces
                |. symbol binarySymbol
                |. spaces
                |= lazy (\_ -> expr)
    in
    oneOf
        [ makeBinaryParser "<" Lt
        , makeBinaryParser "<=" Lte
        , makeBinaryParser "==" Eq
        , makeBinaryParser ">=" Mte
        , makeBinaryParser ">" Mt
        , makeBinaryParser "&&" And
        , makeBinaryParser "||" Or
        ]



{-
   true
   false
   unitTotal == 9 && (%unit'A2') >= 1
   unitTotal >= 10 && unitTotal <=11
   unitTotal >= 19
   unitTotal >= 0 && unitTotal <= 2
   unitTotal == 9 && (%unit'B2') >= 1
   unitTotal >= 19 && unitTotal <= 22
   correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 0 && correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 8 && correct <= 12 && (%'C[1-2]') >= 1
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') <= 0
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') >= 1
-}
