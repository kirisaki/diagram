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

toLength : String -> Result Error Length
toLength s = run lengthDecoder s
            
lengthDecoder : Parser Length
lengthDecoder =
    succeed Length 
        |. spaces
        |= number
        |= unit
        |. spaces
        |. end

number : Parser Float
number =
    oneOf
    [ float
    , integer
    ]
    |. spaces
       |> andThen floatParser

integer : Parser String
integer =
    keep oneOrMore isDigit

float : Parser String
float = delayedCommit (ignore oneOrMore isDigit)
      ( ignore zeroOrMore isDigit
      |. symbol "."
      |. ignore oneOrMore isDigit
      )
      |> source

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
unitString = keep zeroOrMore <| toLower >> isLower

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
        _ -> Px

unitToString : Unit -> String
unitToString u =
    case u of
        Em -> "em"
        Ex -> "ex"
        In -> "in"
        Cm -> "cm"
        Mm -> "mm"
        Pt -> "pt"
        Pc -> "pc"
        Px -> "px"
        Ch -> "ch"
        Rem -> "rem"
        Vw -> "vw"
        Vh -> "vh"
        Vmin -> "vmin"
        Vmax -> "vmax"
        Q -> "q"
        
