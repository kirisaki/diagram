module Diagram exposing (..)

import Array exposing (Array)
import Svg
import Svg.Attributes as Attr 

import Diagram.Unit exposing (Length, Unit(..))

type Shape = Rectangle
           | Oval
    

type alias Node =
    { label: String
    }

type Edge = Edge Node Node

type alias PreRenderNode =
    { label: String
    , x: Length
    , y: Length
    , width: Length
    , height: Length
    }

type Graph = Graph (Array Node) (Array Edge)

render : Graph -> Svg.Svg msg
render (Graph ns es) = Svg.svg
                       [ Attr.width "300"
                       , Attr.height "300"
                       , Attr.fontSize "10pt"
                       ]
                       <| Array.toList <| Array.map renderNode ns

renderNode : Node -> Svg.Svg msg
renderNode n =
    Svg.svg
        [ Attr.width <| flip (++) "em" <| toString <| String.length n.label
        , Attr.height "3em"
        ]
        [ Svg.rect
          [ Attr.width "100%"
          , Attr.height "100%"
          , Attr.fill "white"
          , Attr.stroke "black"
          , Attr.strokeWidth "1"
          ]
              []
        , Svg.text_
            [ Attr.x "50%"
            , Attr.y "55%"
            , Attr.textAnchor "middle"
            , Attr.fontSize "1em"
            ]
              [ Svg.text n.label ]
        ]
