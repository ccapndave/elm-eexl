module Eexl.Parse exposing (parse)

import Eexl.Context as Context exposing (Context)
import Eexl.Eval as Eval exposing (T(..))
import List.Extra
import Parser exposing (..)
import Set


type Operator
    = BinaryOp String BinaryOpDef
    | LParenOperator
    | RParenOperator


type Assoc
    = AssocNone
    | AssocLeftToRight
    | AssocRightToLeft


type alias BinaryOpDef =
    { parser : Parser ()
    , precedence : Int
    , assoc : Assoc
    , eval : T -> T -> Result String T
    }


operators : List Operator
operators =
    [ BinaryOp "^" { parser = symbol "^", precedence = 13, assoc = AssocRightToLeft, eval = Eval.exp }
    , BinaryOp "*" { parser = symbol "*", precedence = 12, assoc = AssocLeftToRight, eval = Eval.mul }
    , BinaryOp "+" { parser = symbol "+", precedence = 11, assoc = AssocLeftToRight, eval = Eval.add }
    , BinaryOp "<=" { parser = backtrackable <| symbol "<=", precedence = 9, assoc = AssocNone, eval = Eval.lte }
    , BinaryOp "<" { parser = symbol "<", precedence = 9, assoc = AssocNone, eval = Eval.lt }
    , BinaryOp ">=" { parser = backtrackable <| symbol ">=", precedence = 9, assoc = AssocNone, eval = Eval.mte }
    , BinaryOp ">" { parser = symbol ">", precedence = 9, assoc = AssocNone, eval = Eval.mt }
    , BinaryOp "==" { parser = symbol "==", precedence = 8, assoc = AssocLeftToRight, eval = Eval.eq }
    , BinaryOp "&&" { parser = symbol "&&", precedence = 4, assoc = AssocLeftToRight, eval = Eval.and }
    , BinaryOp "||" { parser = symbol "||", precedence = 3, assoc = AssocLeftToRight, eval = Eval.or }
    , LParenOperator
    , RParenOperator
    ]


getPrecedenceAndAssociativity : List Operator -> Operator -> Maybe ( Int, Assoc )
getPrecedenceAndAssociativity operators_ operator_ =
    operators_
        |> List.filter
            (\op ->
                case ( op, operator_ ) of
                    ( BinaryOp n1 _, BinaryOp n2 _ ) ->
                        n1 == n2

                    ( LParenOperator, LParenOperator ) ->
                        True

                    ( RParenOperator, RParenOperator ) ->
                        True

                    otherwise ->
                        False
            )
        |> List.head
        |> Maybe.andThen
            (\op ->
                case op of
                    BinaryOp _ { precedence, assoc } ->
                        Just ( precedence, assoc )

                    otherwise ->
                        Nothing
            )


parse : Context -> String -> Result (List DeadEnd) T
parse context string =
    run (expression context) string


int : Parser Int
int =
    oneOf
        [ succeed negate
            |. symbol "-"
            |= Parser.int
        , Parser.int
        ]


bool : Parser Bool
bool =
    oneOf
        [ map (\_ -> True) (keyword "true")
        , map (\_ -> False) (keyword "false")
        ]



-- not : Context -> Parser Token
-- not context =
--     succeed Not
--         |. symbol "!"
--         |= lazy (\_ -> expression context)


var : Context -> Parser Int
var context =
    succeed identity
        |= variable
            { start = Char.isLower
            , inner = Char.isAlphaNum
            , reserved = Set.empty
            }
        |. spaces
        |> andThen
            (\name ->
                Context.getConstant name context
                    |> Maybe.map succeed
                    |> Maybe.withDefault (problem <| "Unknown variable '" ++ name ++ "'")
            )


operator : Parser Operator
operator =
    operators
        |> List.map
            (\op ->
                case op of
                    BinaryOp _ { parser } ->
                        map (\_ -> op) parser

                    LParenOperator ->
                        map (\_ -> LParenOperator) (symbol "(")

                    RParenOperator ->
                        map (\_ -> RParenOperator) (symbol ")")
            )
        |> oneOf


operatorHelp : Operator -> ( List T, List Operator ) -> Parser ( List T, List Operator )
operatorHelp op ( exprStack, operatorStack ) =
    let
        {- There is a function at the top of the operator stack -}
        functionAtTopOfStack : List Operator -> Bool
        functionAtTopOfStack stack =
            -- List.head stack == FunctionOperator -- FUNCTIONS IMPLEMENTED USING SUBSTITUTION FOR THE MOMENT
            False

        {- There is an operator at the top of the operator stack with greater precedence OR
           The operator at the top of the operator stack has equal precedence and is left associative
        -}
        higherPrecedenceAtTopOfStack : List Operator -> Operator -> Bool
        higherPrecedenceAtTopOfStack stack currentOperator =
            Maybe.map2
                (\( opPrecedence, opAssociativity ) ( stackPrecedence, stackAssociativity ) ->
                    (stackPrecedence > opPrecedence)
                        || (stackPrecedence == opPrecedence && stackAssociativity == AssocLeftToRight)
                )
                (getPrecedenceAndAssociativity operators currentOperator)
                (List.head stack |> Maybe.andThen (getPrecedenceAndAssociativity operators))
                |> Maybe.withDefault False

        {- The operator at the top of the operator stack is not a left bracket -}
        leftParentAtTopOfStack : List Operator -> Bool
        leftParentAtTopOfStack stack =
            List.head stack == Just LParenOperator
    in
    {- while (condition) -}
    if (functionAtTopOfStack operatorStack || higherPrecedenceAtTopOfStack operatorStack op) && not (leftParentAtTopOfStack operatorStack) then
        {- Apply an operator from the operator stack to the output queue and recurse -}
        case ( exprStack, operatorStack ) of
            ( rhs :: lhs :: es, (BinaryOp _ topOperator) :: os ) ->
                case applyBinaryOp topOperator lhs rhs of
                    Ok t ->
                        operatorHelp op ( t :: es, os )

                    Err err ->
                        problem err

            otherwise ->
                problem "This can't happen AFAIK"

    else
        {- Push it onto the operator stack. -}
        succeed ( exprStack, op :: operatorStack )


func : Context -> Parser Int
func context =
    succeed Tuple.pair
        |= backtrackable
            (variable
                { start = Char.isLower
                , inner = Char.isAlphaNum
                , reserved = Set.empty
                }
            )
        |. backtrackable (symbol "(")
        |. symbol "\""
        |= getChompedString (chompWhile (\c -> c /= '"'))
        |. symbol "\""
        |. symbol ")"
        |> andThen
            (\( name, arg ) ->
                Context.getFunction name context
                    |> Maybe.map (\fn -> succeed <| fn arg)
                    |> Maybe.withDefault (problem <| "Unknown function '" ++ name ++ "'")
            )


rParenHelp : ( List T, List Operator ) -> Parser ( List T, List Operator )
rParenHelp ( exprStack, operatorStack ) =
    case ( exprStack, operatorStack ) of
        ( es, LParenOperator :: os ) ->
            succeed ( es, os )

        ( rhs :: lhs :: es, (BinaryOp _ topOperator) :: os ) ->
            case applyBinaryOp topOperator lhs rhs of
                Ok t ->
                    rParenHelp ( t :: es, os )

                Err err ->
                    problem err

        otherwise ->
            problem "Unable to parse parenthesis"


endHelp : ( List T, List Operator ) -> Parser T
endHelp ( exprStack, operatorStack ) =
    case ( exprStack, operatorStack ) of
        ( _, LParenOperator :: os ) ->
            problem "Mismatched left parenthesis"

        ( _, RParenOperator :: os ) ->
            problem "Mismatched right parenthesis"

        ( rhs :: lhs :: es, (BinaryOp _ topOperator) :: os ) ->
            case applyBinaryOp topOperator lhs rhs of
                Ok t ->
                    endHelp ( t :: es, os )

                Err err ->
                    problem err

        ( [ expr ], [] ) ->
            succeed expr

        otherwise ->
            problem ""


expression : Context -> Parser T
expression context =
    expressionHelp context ( [], [] )


expressionHelp : Context -> ( List T, List Operator ) -> Parser T
expressionHelp context ( exprStack, operatorStack ) =
    -- let
    --     _ =
    --         Debug.log "stacks" ( exprStack, operatorStack )
    -- in
    succeed identity
        |. spaces
        |= oneOf
            [ {- if the token is a number, then: -}
              backtrackable int
                {- push it to the output queue. -}
                |> andThen
                    (\i -> expressionHelp context ( IntT i :: exprStack, operatorStack ))

            {- if the token is a bool, then: -}
            , backtrackable bool
                {- push it to the output queue. -}
                |> andThen
                    (\i -> expressionHelp context ( BoolT i :: exprStack, operatorStack ))

            {- if the token is a var, then: -}
            , backtrackable (var context)
                {- push it to the output queue. -}
                |> andThen
                    (\i -> expressionHelp context ( IntT i :: exprStack, operatorStack ))

            {- if the token is a func, then: -}
            , backtrackable (func context)
                {- push it to the output queue. -}
                |> andThen
                    (\i -> expressionHelp context ( IntT i :: exprStack, operatorStack ))

            {- if the token is an operator, then: -}
            , backtrackable operator
                |> andThen
                    (\op ->
                        operatorHelp op ( exprStack, operatorStack )
                            |> andThen (expressionHelp context)
                    )

            {- if the token is a left bracket (i.e. "("), then: -}
            , backtrackable (symbol "(")
                {- push it onto the operator stack. -}
                |> andThen
                    (\i -> expressionHelp context ( exprStack, LParenOperator :: operatorStack ))

            {- if the token is a right bracket (i.e. ")"), then: -}
            , backtrackable (symbol ")")
                |> andThen
                    (\_ ->
                        rParenHelp ( exprStack, operatorStack )
                            |> andThen (expressionHelp context)
                    )

            {- if we are at the end of the input -}
            , end |> andThen (\_ -> endHelp ( exprStack, operatorStack ))
            ]
        |. spaces


applyBinaryOp : BinaryOpDef -> T -> T -> Result String T
applyBinaryOp { eval } lhs rhs =
    eval lhs rhs
