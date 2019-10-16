module RPN exposing (calc)


type OperatorType
    = Add
    | Sub
    | Mul
    | Div


type Token
    = Value Float
    | Operator OperatorType
    | Impossible


{-| Interpet a Reverse Polish Notation mathematical expression.

Reverse Polish Notation is a notation for mathematical expressions, that uses
the concept of a "stack". Whenever an operator (+, -, \*, or /) is encountered,
the 2 next elements of the "stack" are retrieved, and the result is pushed
back onto the stack. If one of those 2 previous elements is also an operator,
we recursively treat it starting from there, all the way until we have the
condition with an operator and 2 values.

Example: "1 2 3 + /"

1.  We turn it into a list, and flip it:
    [ "/", "+", "3", "2", "1" ]

2.  We process the stack, taking 1 element:
      - ( x :: xs ) -> ( "/" :: [ "+", "3", "2", "1" ] )
      - x is an operator, we need 2 values to apply it.

3.  Take the next 2 elements:
      - ( a :: b :: xs ) -> ( "+" :: "3" :: [ "2", "1" ] )
      - a is an operator.
      - Recursively treat this as a new stack.

4.  We process the new stack, taking 1 element:
      - ( y :: ys ) -> ( "+" :: [ "3", "2", "1" ] )
      - y is an operator, we need 2 values to apply it.

5.  Take the next 2 elements:
      - ( b :: c :: ys ) -> ( "3" :: "2" :: [ "1" ] )
      - We have 2 values. Apply the previous operator, y -> 3 + 2 = 5
      - Push this back onto the stack -> [ "5", "1" ]

6.  Process the stack again from point 2:
      - ( x :: xs ) -> ( "/" :: [ "5", "1" ] )
      - x is an operator, we need 2 values to apply it.

7.  Take the next 2 elements:
      - ( d :: e :: xs ) -> ( "5" :: "1" :: [] )
      - We have 2 values. Apply the previous operator, x -> 5 / 1 = 5
      - Push this back onto the stack -> [ "5" ]

8.  We process the stack, taking 1 element:
      - ( x :: xs ) -> ( "5" :: [] )
      - We have only 1 token left in the stack. This is our result -> 5

-}
calc : String -> Maybe Float
calc expression =
    let
        list =
            parse expression
    in
    if List.member Impossible list then
        Nothing

    else
        case compute (List.reverse list) of
            -- If there's only a single value left, that's our result.
            [ Value n ] ->
                Just n

            -- Anything else is an error.
            _ ->
                Nothing


{-| Perform the computation on a list of tokens.

Take a list of tokens, and reduce it to -- hopefully -- a single Value. If the
result is a list containing anything else than a single Value token, the
computation has failed, and the expression wasn't computable.

-}
compute : List Token -> List Token
compute list =
    case list of
        -- If we have an operator, followed by 2 values, we can perform the
        -- operation.
        (Operator op) :: (Value b) :: (Value a) :: xs ->
            compute (Value (apply op a b) :: xs)

        -- If we have 2 operators following each other, we need to compute the
        -- rest of the stack first, from the 2nd operator onwards.
        (Operator op1) :: (Operator op2) :: xs ->
            compute (Operator op1 :: compute (Operator op2 :: xs))

        -- If we have 2 operators, with a value between, we need to compute the
        -- rest of the stack first, from the 2nd operator onwards.
        (Operator op1) :: (Value a) :: (Operator op2) :: xs ->
            compute (Operator op1 :: Value a :: compute (Operator op2 :: xs))

        -- Any other pattern must just be returned as-is.
        _ ->
            list


{-| Helper function to apply an Operator to 2 Values.
-}
apply : OperatorType -> Float -> Float -> Float
apply op a b =
    case op of
        Add ->
            a + b

        Sub ->
            a - b

        Mul ->
            a * b

        Div ->
            a / b


{-| Helper function for parsing an expression to a list of tokens.

Take a space-separated string of values, and return a list of tokens. Each token
represents either a Value, or an Operator. If a value cannot be parsed, it will
be represented by an Impossible token.

-}
parse : String -> List Token
parse expression =
    String.split " " expression
        |> List.map
            (\item ->
                case String.uncons item of
                    Just ( '+', _ ) ->
                        Operator Add

                    Just ( '-', _ ) ->
                        Operator Sub

                    Just ( '*', _ ) ->
                        Operator Mul

                    Just ( '/', _ ) ->
                        Operator Div

                    _ ->
                        case String.toFloat item of
                            Just n ->
                                Value n

                            Nothing ->
                                Impossible
            )
