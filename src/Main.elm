module Main exposing (main)

import Browser exposing (Document)
import Board exposing (Board)
import Html exposing (div)
import Random

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : () -> ( Model, Cmd Msg)
init _ =
    ( { board = Board.emptyBoard 
      , title = "Elm 15"
      }
    , Random.generate BoardGenerated Board.generator
    )

type Msg 
    = BoardGenerated Board
    | NoOp

type alias Model = 
    { board : Board
    , title : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

view : Model -> Document Msg
view model =
    { title = model.title
    , body = [div [][]]
    }
