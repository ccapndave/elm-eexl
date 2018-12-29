module Eexl.Eexl exposing (evaluateBool, evaluateInt)

{-| This library allows the evaluation of expressions at runtime.

@docs evaluateBool, evaluateInt

-}

import Eexl.Context exposing (Context)
import Eexl.Eval as Eval
import Eexl.Parse as Parse
import Parser exposing (DeadEnd, Problem(..))


{-| Evaluate an expression that returns a Bool.
-}
evaluateBool : Context -> String -> Result String Bool
evaluateBool context str =
    str
        |> Parse.parse context
        |> Result.mapError deadEndsToString
        |> Result.andThen (Eval.tToBool >> Result.fromMaybe "Result is not a Bool")


{-| Evaluate an expression that returns an Int.
-}
evaluateInt : Context -> String -> Result String Int
evaluateInt context str =
    str
        |> Parse.parse context
        |> Result.mapError deadEndsToString
        |> Result.andThen (Eval.tToInt >> Result.fromMaybe "Result is not an Int")


{-| Taken from unmerged pull request at <https://github.com/elm/parser/pull/16>
-}
deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.concat (List.intersperse "; " (List.map deadEndToString deadEnds))


deadEndToString : DeadEnd -> String
deadEndToString deadend =
    problemToString deadend.problem ++ " at row " ++ String.fromInt deadend.row ++ ", col " ++ String.fromInt deadend.col


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol s ->
            "expecting symbol '" ++ s ++ "'"

        ExpectingKeyword s ->
            "expecting keyword '" ++ s ++ "'"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem s ->
            "problem " ++ s

        BadRepeat ->
            "bad repeat"
