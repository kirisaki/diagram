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
suiteToLength = describe "toLength"
                [ describe "passible" <|
                      List.map generateTestCase
                      [ ( "normal case", "1em", Length 1 Em )
                      , ( "float value", "0.1cm", Length 0.1 Cm)
                      , ( "including spaces", " 0.12 pc ", Length 0.12 Pc )
                      , ( "no integer part", ".34em", Length 0.34 Em )
                      , ( "no unit", "13", Length 13 Px )
                      , ( "invalid unit", "30 nyaan", Length 30 Px)
                      ]
                , describe "unpassible" <|
                    [ test "too many colon" <|
                          \_ -> toLength "0.2.3mm" |> err
                    , test "colon bifore uniy" <|
                          \_ -> toLength "2.px" |> err
                    ]
                ]
                 
                
