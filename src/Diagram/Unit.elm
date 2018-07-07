module Diagram.Unit exposing
    (..)

import Parser exposing
    ( Parser
    , Error
    , Count(..)
    , (|.)
    , (|=)
    , symbol
    , end
    , delayedCommit
    , keep
    , succeed
    , fail
    , andThen
    , oneOf
    , source
    , ignore
    , zeroOrMore
    , oneOrMore
    , run)
import Char exposing
    ( toLower
    , isLower
    , isDigit)

type Length = Length Float Unit

type alias Point =
    { x: Length
    , y: Length
    }

type Unit = Em
          | Ex
          | In
          | Cm
          | Mm
          | Pt
          | Pc
          | Px
          | Ch
          | Rem
          | Vw
          | Vh
          | Vmin
          | Vmax
          | Q
          | Percent

toLength : String -> Result Error Length
toLength s = run lengthDecoder s

toString : Length -> String
toString (Length v u) = Basics.toString v ++ (unitToString u)

lengthDecoder : Parser Length
lengthDecoder =
    succeed identity
        |. spaces
        |= oneOf
           [ float
           , integer
           ]
        |. spaces
        |. end

       

integer : Parser Length
integer = 
    succeed Length 
        |= (keep oneOrMore isDigit |> andThen floatParser)
        |. spaces
        |= unit

float : Parser Length
float = 
    succeed Length
        |= ( delayedCommit (ignore zeroOrMore isDigit)
                 ( succeed identity
                 |. ignore zeroOrMore isDigit
                 |. symbol "."
                 |. ignore oneOrMore isDigit)
           |> source |> andThen floatParser
           )
        |. spaces
        |= unit

floatParser : String -> Parser Float
floatParser s =
    case String.toFloat s of
        Ok n ->
            succeed n
        Err e ->
            fail e

unit : Parser Unit
unit = Parser.map stringToUnit unitString

unitString : Parser String
unitString = keep zeroOrMore <| toLower >> (\c -> isLower c || c == '%')

spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')
            
stringToUnit : String -> Unit
stringToUnit s =
    case String.toLower s of
        "em" -> Em
        "ex" -> Ex
        "in" -> In
        "cm" -> Cm
        "mm" -> Mm
        "pt" -> Pt
        "pc" -> Pc
        "ch" -> Ch
        "rem" -> Rem
        "vw" -> Vw
        "vh" -> Vh
        "vmin" ->Vmin
        "vmax" ->Vmax
        "q" -> Q
        "%" -> Percent
        _ -> Px

unitToString : Unit -> String
unitToString u =
    if Basics.toString u == "Percent" then
        "%"
    else
        String.toLower <| Basics.toString u
