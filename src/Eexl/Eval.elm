module Eexl.Eval exposing (T(..), add, and, eq, exp, lt, lte, mt, mte, mul, not, or, tToBool, tToInt)

import Result.FlatMap as Result


type T
    = BoolT Bool
    | IntT Int


tToBool : T -> Maybe Bool
tToBool t =
    case t of
        BoolT b ->
            Just b

        otherwise ->
            Nothing


tToInt : T -> Maybe Int
tToInt t =
    case t of
        IntT i ->
            Just i

        otherwise ->
            Nothing



{- evalBool : Expr -> Result String Bool
   evalBool expr =
       eval expr
           |> Result.andThen
               (\t ->
                   case t of
                       BoolT b ->
                           Ok b

                       otherwise ->
                           Err "This expression does not return a bool"
               )


   evalInt : Expr -> Result String Int
   evalInt expr =
       eval expr
           |> Result.andThen
               (\t ->
                   case t of
                       IntT i ->
                           Ok i

                       otherwise ->
                           Err "This expression does not return an int"
               )


   eval : Expr -> Result String T
   eval expr =
       case expr of
           Int i ->
               Ok <| IntT i

           Bool b ->
               Ok <| BoolT b

           Not e ->
               Result.andThen not (eval e)

           Exp e1 e2 ->
               Result.flatMap2 exp (eval e1) (eval e2)

           Add e1 e2 ->
               Result.flatMap2 add (eval e1) (eval e2)

           Mul e1 e2 ->
               Result.flatMap2 mul (eval e1) (eval e2)

           Lte e1 e2 ->
               Result.flatMap2 lte (eval e1) (eval e2)

           Lt e1 e2 ->
               Result.flatMap2 lt (eval e1) (eval e2)

           Mte e1 e2 ->
               Result.flatMap2 mte (eval e1) (eval e2)

           Mt e1 e2 ->
               Result.flatMap2 mt (eval e1) (eval e2)

           Eq e1 e2 ->
               Result.flatMap2 eq (eval e1) (eval e2)

           And e1 e2 ->
               Result.flatMap2 and (eval e1) (eval e2)

           Or e1 e2 ->
               Result.flatMap2 or (eval e1) (eval e2)
-}


not : T -> Result String T
not t =
    case t of
        BoolT b ->
            Ok <| BoolT (Basics.not b)

        otherwise ->
            Err "Type error: !"


exp : T -> T -> Result String T
exp t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| IntT (i1 ^ i2)

        otherwise ->
            Err "Type error: +"


add : T -> T -> Result String T
add t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| IntT (i1 + i2)

        otherwise ->
            Err "Type error: +"


mul : T -> T -> Result String T
mul t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| IntT (i1 * i2)

        otherwise ->
            Err "Type error: *"


lte : T -> T -> Result String T
lte t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| BoolT (i1 <= i2)

        otherwise ->
            Err "Type error: <="


lt : T -> T -> Result String T
lt t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| BoolT (i1 < i2)

        otherwise ->
            Err "Type error: <"


mte : T -> T -> Result String T
mte t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| BoolT (i1 >= i2)

        otherwise ->
            Err "Type error: >="


mt : T -> T -> Result String T
mt t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| BoolT (i1 > i2)

        otherwise ->
            Err "Type error: >"


eq : T -> T -> Result String T
eq t1 t2 =
    case ( t1, t2 ) of
        ( IntT i1, IntT i2 ) ->
            Ok <| BoolT (i1 == i2)

        ( BoolT b1, BoolT b2 ) ->
            Ok <| BoolT (b1 == b2)

        otherwise ->
            Err "Type error: =="


and : T -> T -> Result String T
and t1 t2 =
    case ( t1, t2 ) of
        ( BoolT b1, BoolT b2 ) ->
            Ok <| BoolT (b1 && b2)

        otherwise ->
            Err "Type error: &&"


or : T -> T -> Result String T
or t1 t2 =
    case ( t1, t2 ) of
        ( BoolT b1, BoolT b2 ) ->
            Ok <| BoolT (b1 || b2)

        otherwise ->
            Err "Type error: ||"
