module ParserTests exposing (suite)

import Eexl.Ast exposing (..)
import Eexl.Parser exposing (parse)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "Eexl"
        [ literalTests
        , unaryOpTests
        , binaryOpTests
        , complexTests
        ]


literalTests : Test
literalTests =
    describe "literals"
        [ only <|
            test "true" <|
                \_ -> Expect.equal (Ok <| Literal <| Bool True) (parse "true")
        , test "false" <|
            \_ -> Expect.equal (Ok <| Literal <| Bool False) (parse "false")
        , fuzz int "int" <|
            \n -> Expect.equal (Ok <| Literal <| Int n) (parse (String.fromInt n))
        , test "var" <|
            \_ -> Expect.equal (Ok <| Literal <| Var "someVar") (parse "someVar")
        ]


unaryOpTests : Test
unaryOpTests =
    describe "unaryOp"
        [ test "not bool" <|
            \_ -> Expect.equal (Ok <| UnaryOp <| Not <| Literal <| Bool True) (parse "!true")
        , test "false" <|
            \_ -> Expect.equal (Ok <| UnaryOp <| Not <| Literal <| Var "x") (parse "!x")
        , test "multiple" <|
            \_ -> Expect.equal (Ok <| UnaryOp <| Not <| UnaryOp <| Not <| Literal <| Var "x") (parse "!!x")
        ]


binaryOpTests : Test
binaryOpTests =
    describe "binaryOp"
        [ test "eq" <|
            \_ -> Expect.equal (Ok <| BinaryOp <| Eq (Literal <| Int 1) (Literal <| Int 2)) (parse "1 == 2")
        ]


complexTests : Test
complexTests =
    skip <|
        describe "complex"
            []
