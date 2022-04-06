module Main exposing (..)

import Array exposing (..)
import GameSeeds exposing (..)
import Helpers exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import View exposing (..)
import Browser
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> (Model, Cmd msg)
init _ =
    ({ isPaused = True
      , cells = GameSeeds.middlerows numRows numCols
      , stepDelay = 1000
      , generation = 1
      }
      , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isPaused then
        Sub.batch []
    else
        Time.every 1000 AutoTickGame
        --[ Time.every ((toFloat model.stepDelay) * Time.millisecond) AutoTickGame ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        newModel =
            case msg of
                TogglePaused ->
                    { model | isPaused = not model.isPaused }

                ToggleCell idx ->
                    { model | cells = toggleCell idx model.cells }

                TickGame ->
                    tickGameWrapper model

                AutoTickGame _ ->
                    tickGameWrapper model
    in
        (newModel, Cmd.none)


tickGameWrapper : Model -> Model
tickGameWrapper model =
    let
        newcells =
            tickGame model

        newpaused =
            if model.cells == newcells then
                True

            else if cellsAlive model == 0 then
                True

            else
                model.isPaused
    in
    { model
        | cells = newcells
        , generation = model.generation + 1
        , isPaused = newpaused
    }


tickGame : Model -> Array Bool
tickGame model =
    Array.indexedMap
        (\n val -> tickCell n model)
        model.cells


tickCell : Int -> Model -> Bool
tickCell idx model =
    let
        neighborsAlive =
            getNeighborIdxs idx
                |> List.map (\n -> Array.get n model.cells)
                |> List.filter (\n -> n == Just True)
                |> List.length
    in
    case Array.get idx model.cells of
        Nothing ->
            False

        Just True ->
            neighborsAlive == 2 || neighborsAlive == 3

        Just False ->
            neighborsAlive == 3


getNeighborIdxs : Int -> List Int
getNeighborIdxs idx =
    let
        ( row, col ) =
            toRowCol idx

        leftneighbors =
            if col == 0 then
                []

            else
                [ idx - 1, (idx - numCols) - 1, (idx + numCols) - 1 ]

        midneighbors =
            [ idx - numCols, idx + numCols ]

        rightneighbors =
            if col == numCols - 1 then
                []

            else
                [ idx - numCols + 1, idx + 1, idx + numCols + 1 ]
    in
    List.filter
        (\n -> n >= 0 && n < numRows * numCols)
        (leftneighbors ++ midneighbors ++ rightneighbors)
