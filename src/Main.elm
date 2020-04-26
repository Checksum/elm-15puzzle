module Main exposing (main)

import Board exposing (Board)
import Browser
import Html exposing (Html, div, text)
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.emptyBoard
      , initialised = False
      , title = "Elm 15"
      }
    , Random.generate BoardGenerated Board.generator
    )


type Msg
    = BoardGenerated Board
    | NoOp


type alias Model =
    { board : Board
    , initialised : Bool
    , title : String
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardGenerated board ->
            if Board.isSolvable board then
                ( { model | board = board, initialised = True }, Cmd.none )

            else
                ( model, Random.generate BoardGenerated Board.generator )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] []
