module Diagram.Unit exposing ( Length(..)
                     , Unit(..)
                     , stringToUnit
                     , unitToString
                     )

type Length = Length Float Unit

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
        
