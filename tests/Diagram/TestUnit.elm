module Diagram.TestUnit exposing (..)

import Diagram.Unit exposing (Length(..), Unit(..), toLength)
import Parser
import Expect exposing (..)
import Test exposing (..)


equalLength : Length -> Result Parser.Error Length -> Expectation
equalLength (Length en eu) result =
    case result of
        Ok (Length rn ru) ->
            if ru /= eu then
                fail "different unit"
            else
                rn |> within (Absolute 0.0001) en
        Err _ ->
            fail "parse error"

generateTestCase : ( String ,String, Length ) -> Test
generateTestCase ( desc, input, expected ) =
    test desc <|
        \_ ->
            toLength input |> equalLength expected
                   

suiteToLength : Test
suiteToLength = describe "toLength" <| List.map generateTestCase
                [ ( "normal case", "1em", Length 1 Em )
                , ( "float value", "0.1cm", Length 0.1 Cm)
                ]
                 
                
