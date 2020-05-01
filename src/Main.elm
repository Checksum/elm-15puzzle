module Main exposing (main)

import Board exposing (Board)
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    | MoveTile Board.Pos


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
                ( { model | board = board, initialised = True }
                , Cmd.none
                )

            else
                ( model, Random.generate BoardGenerated Board.generator )

        MoveTile pos ->
            ( { model | board = Board.move pos model.board }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ if not model.initialised then
            div [] [ text "Generating board" ]

          else
            viewBoard model
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        boardSize =
            "calc(var(--cell-size) * " ++ String.fromInt Board.size ++ " + 2px)"

        list =
            Board.toList model.board
    in
    div [ class "board", style "width" boardSize, style "height" boardSize ] (List.map (viewCell model) list)


viewCell : Model -> ( Board.Pos, Maybe Board.Tile ) -> Html Msg
viewCell model ( pos, tile ) =
    let
        cellNum =
            case tile of
                Just n ->
                    String.fromInt n

                Nothing ->
                    ""

        cssClass =
            "tile "
                ++ (if pos == model.board.empty then
                        "empty"

                    else
                        ""
                   )
    in
    div
        [ class cssClass
        , onClick (MoveTile pos)
        ]
        [ text cellNum ]
