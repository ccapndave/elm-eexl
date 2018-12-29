module ParseTests exposing (suite)

import Eexl.Context as Context exposing (Context)
import Eexl.Eexl exposing (evaluateBool, evaluateInt)
import Eexl.Parse exposing (parse)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Problem(..))
import Test exposing (..)


suite : Test
suite =
    describe "Eexl"
        [ literalTests
        , mathsTests
        , boolTests
        , varTests
        , funcTests
        , realWorldTests
        ]



{-
   unaryOpTests : Test
   unaryOpTests =
       describe "unaryOp"
           [ test "not bool" <|
               \_ -> Expect.equal (Ok <| Not <| Bool True) (parse Context.empty "!true")
           , test "multiple" <|
               \_ -> Expect.equal (Ok <| Not <| Not <| Bool True) (parse Context.empty "!!true")
           ]
-}


literalTests : Test
literalTests =
    describe "literals"
        [ test "true" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "true")
        , test "false" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty "false")
        , fuzz int "int" <|
            \n ->
                Expect.equal
                    (Ok n)
                    (evaluateInt Context.empty <| String.fromInt n)
        ]


mathsTests : Test
mathsTests =
    describe "Maths"
        [ test "1 + 1" <|
            \_ ->
                Expect.equal
                    (Ok 2)
                    (evaluateInt Context.empty "1 + 1")
        , test "1 + 2 + 3" <|
            \_ ->
                Expect.equal
                    (Ok 6)
                    (evaluateInt Context.empty "1 + 2 + 3")
        , test "100 * 3 + 2" <|
            \_ ->
                Expect.equal
                    (Ok 302)
                    (evaluateInt Context.empty "100 * 3 + 2")
        , test "100 * (3 + 2)" <|
            \_ ->
                Expect.equal
                    (Ok 500)
                    (evaluateInt Context.empty "100 * (3 + 2)")
        ]


boolTests : Test
boolTests =
    describe "Bool"
        [ test "true" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "true")
        , test "1 + 2 < 4" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "1 + 2 < 4")
        , test "1 + 2 * 3 < 4 + 3" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty "1 + 2 * 3 < 4 + 3")
        ]


varTests : Test
varTests =
    describe "varTests"
        [ test "non-existant var" <|
            \_ ->
                Expect.err
                    (evaluateInt Context.empty "1 + x")
        , test "existing var" <|
            \_ ->
                Expect.equal
                    (Ok 2)
                    (evaluateInt (Context.empty |> Context.addConstant "x" 1) "1 + x")
        ]


funcTests : Test
funcTests =
    let
        stringToInt : String -> Int
        stringToInt =
            String.toInt >> Maybe.withDefault -1
    in
    describe "funcTests"
        [ test "non-existant function" <|
            \_ ->
                Expect.err
                    (evaluateInt Context.empty "stringToInt(\"something\")")
        , test "function 1" <|
            \_ ->
                Expect.equal
                    (Ok 1)
                    (evaluateInt (Context.empty |> Context.addFunction "stringToInt" stringToInt) "stringToInt(\"1\")")
        , test "function 2" <|
            \_ ->
                Expect.equal
                    (Ok -1)
                    (evaluateInt (Context.empty |> Context.addFunction "stringToInt" stringToInt) "stringToInt(\"something else\")")
        ]


realWorldTests : Test
realWorldTests =
    let
        makeContext : Int -> Int -> Int -> Context
        makeContext unitTotal correct funcReturns =
            Context.empty
                |> Context.addConstant "unitTotal" unitTotal
                |> Context.addConstant "correct" correct
                |> Context.addFunction "unitScoreWithTags" (\_ -> funcReturns)
                |> Context.addFunction "unitScoreWithoutTags" (\_ -> funcReturns)
                |> Context.addFunction "exerciseScoreWithTags" (\_ -> funcReturns)
                |> Context.addFunction "exerciseScoreWithoutTags" (\_ -> funcReturns)
    in
    describe "realWorldTests"
        [ test "2 + 3 + 4" <|
            \_ ->
                Expect.equal
                    (Ok 9)
                    (evaluateInt Context.empty """2 + 3 + 4""")
        , test "1 == 2 && 3 == 4" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty """1 == 2 && 3 == 4""")
        , test "1 + 2 < 4" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty """1 + 2 < 4""")
        , test "1 + 2 * 3" <|
            \_ ->
                Expect.equal
                    (Ok 9)
                    (evaluateInt Context.empty """1 + 2 * 4""")
        , test "1 + 2 * 3 < 4 + 3" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool Context.empty "1 + 2 * 3 < 4 + 3")
        , test "2 + 3 ^ 2 * 3 + 4" <|
            \_ ->
                Expect.equal
                    (Ok 33)
                    (evaluateInt Context.empty "2 + 3 ^ 2 * 3 + 4")
        , test "2 + 3 * 4 + 5 == 19" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool Context.empty "2 + 3 * 4 + 5 == 19")
        , test "1.1" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 3 0 0) """unitTotal == 9 && unitScoreWithTags("A2") >= 1""")
        , test "1.2" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 9 0 1) """unitTotal == 9 && unitScoreWithTags("A2") >= 1""")
        , test "2.1" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 9 0 1) """unitTotal >= 10 && unitTotal <= 11""")
        , test "2.2" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 9 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "2.3" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 10 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "2.4" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 11 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "2.5" <|
            \_ ->
                Expect.equal
                    (Ok False)
                    (evaluateBool (makeContext 12 0 0) """unitTotal >= 10 && unitTotal <=11""")
        , test "3.1" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 0 2 0) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        , test "3.2" <|
            \_ ->
                Expect.equal
                    (Ok True)
                    (evaluateBool (makeContext 0 4 0) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        , test "3.3" <|
            \_ ->
                Expect.equal (Ok False)
                    (evaluateBool (makeContext 0 5 0) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        , test "3.4" <|
            \_ ->
                Expect.equal (Ok False)
                    (evaluateBool (makeContext 0 0 1) """correct <= 4 && exerciseScoreWithoutTags("A[1-2]") <= 0""")
        ]



{-
   correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 0 && correct <= 4 && (!%'A[1-2]') <= 0
   correct >= 1 && correct <= 4 && (!%'A[1-2]') >= 1
   correct >= 8 && correct <= 12 && (%'C[1-2]') >= 1
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') <= 0
   unitTotal >= 5 && unitTotal <= 7 && (%unit'C[1-2]') >= 1
-}
{-

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
