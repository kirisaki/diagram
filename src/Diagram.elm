module Diagram exposing (..)

import Array exposing (Array)
import Svg
import Svg.Attributes as Attr 

type alias Node =
    { label: String
    }

type Edge = Edge Node Node

type Graph = Graph (Array Node) (Array Edge)

render : Graph -> Svg.Svg msg
render (Graph ns es) = Svg.svg [] <| Array.toList <| Array.map renderNode ns

renderNode : Node -> Svg.Svg msg
renderNode n =
    Svg.g
        [ Attr.width "100", Attr.height "100"]
        [ Svg.text_
              [ Attr.y "20" ]
              [ Svg.text n.label ]
        ]
