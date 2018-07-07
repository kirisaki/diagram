module App exposing (..)

import Array exposing (empty, fromList)
import Diagram
import Html exposing (Html, div, text, program)

type alias Model =
    String


init : ( Model, Cmd Msg )
init =
     "" ! []

type Msg
    = NoOp

view : Model -> Html Msg
view model =
    div []
        [ Diagram.render (Diagram.Graph (fromList [{label = "nyaaan"}]) empty) ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
