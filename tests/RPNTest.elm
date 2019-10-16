module RPNTest exposing (suite)

import Expect exposing (Expectation)
import RPN
import Test exposing (..)


suite : Test
suite =
    describe "RPN.calc"
        [ describe "should correctly do simple arithmetic"
            [ runTest "1" (Just 1)
            , runTest "1 2 +" (Just 3)
            , runTest "2 1 -" (Just 1)
            , runTest "2 1 *" (Just 2)
            , runTest "4 2 /" (Just 2)
            , runTest "2.5 1.5 +" (Just 4)
            , runTest "3 2 /" (Just 1.5)
            ]
        , describe "should correctly do more advanced arithmetic"
            [ runTest "1 2 3 + *" (Just 5)
            , runTest "1 2 3 * +" (Just 7)
            , runTest "2 2 3 4 * - +" (Just -8)
            , runTest "2 2 * 4 + 2 /" (Just 4)
            ]
        , describe "should correctly return Nothing for incorrect notations"
            [ runTest "1 2 3 +" Nothing
            , runTest "1 +" Nothing
            , runTest "1 2 + *" Nothing
            , runTest "12+*" Nothing
            ]
        ]


runTest : String -> Maybe Float -> Test
runTest expression result =
    test expression (\_ -> RPN.calc expression |> Expect.equal result)
