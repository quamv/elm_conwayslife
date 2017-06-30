module View exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, millisecond)
import Model exposing (..)
import Helpers exposing (..)
import GameSeeds exposing (..)

view : Model -> Html Msg
view model =
    div
        [
            style viewContainerStyle
        ]
        [
            viewHeader model
            ,pauseButton model.isPaused
            ,button
                [onClick TickGame]
                [text "tick game"]
            ,gameboardView model
        ]

viewHeader : Model -> Html Msg
viewHeader model =
    header
        []
        [
            h2 [] [ text "Conway's Game of Life" ]
            ,div [] [ text <| "Generation: " ++ (toString model.generation)]
        ]


gameboardView : Model -> Html Msg
gameboardView model =
    let
        rowNbrs =
            List.range 0 9

        tableRows =
            List.map (\n -> rowView n model) rowNbrs
    in
        table
            [] --[style boardTableStyle]
            tableRows


rowView : Int -> Model -> Html Msg
rowView row model =
    let
        (idx1,idx2) =
            getRowFirstLast row

        idxs =
            List.range idx1 idx2

        rowCells =
            List.map (\n -> viewBoardCell n model) idxs
    in
        tr
            [] -- [style boardRowStyle]
            rowCells


pauseButton isPaused =
    let
        txt = case isPaused of
            False -> "Pause"
            True -> "Resume"
    in
        button
            [onClick TogglePaused]
            [text <| txt]

cellButton : Int -> Bool -> Html Msg
cellButton idx alive =
    button
        [
            onClick (ToggleCell idx)
            ,style <| cellButtonStyle alive
        ]
        []

viewBoardCell : Int -> Model -> Html Msg
viewBoardCell idx model =
    let
        innernode =
            case Array.get idx model.cells of
                Nothing ->
                    [div [] [text "E"]]

                Just bool ->
                    [cellButton idx bool]
    in
        td [] innernode


viewContainerStyle =
    [
        ("margin","0 auto")
        ,("width","600px")
    ]

cellButtonStyle alive =
    let
        bg = case alive of
            True -> "black"
            False -> "none"
    in
        [
            ("background",bg)
            ,("min-height","30px")
            ,("min-width","30px")
        ]

