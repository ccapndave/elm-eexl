module Eexl exposing (eval)

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
{- eval : String -> Context -> Result String Bool -}


eval expr context =
    Err ""
