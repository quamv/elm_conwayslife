module Helpers exposing (..)

import Array exposing (..)
import GameSeeds exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
--import Time exposing (Time, millisecond)


toggleCell : Int -> Array Bool -> Array Bool
toggleCell idx cells =
    if idx > numRows * numCols then
        cells

    else
        case Array.get idx cells of
            Nothing ->
                -- we checked for oob earlier. this should never happen
                cells

            Just oldval ->
                Array.set idx (not oldval) cells


boolToInt : Maybe Bool -> Int
boolToInt mbbool =
    case mbbool of
        Just True ->
            1

        Just False ->
            0

        Nothing ->
            0


toRowCol : Int -> ( Int, Int )
toRowCol idx =
    ( idx // numCols
    , remainderBy numCols idx
    )


getRowFirstLast : Int -> ( Int, Int )
getRowFirstLast row =
    ( row * numCols, ((row + 1) * numCols) - 1 )


getValAt : ( Int, Int ) -> Array Bool -> Bool
getValAt ( row, col ) cells =
    let
        maybebool =
            Array.get
                ((row * numCols) + col)
                cells
    in
    case maybebool of
        Just val ->
            val

        Nothing ->
            False


getValAtIdx : Int -> Array Bool -> Bool
getValAtIdx idx cells =
    getValAt ( idx // numRows, remainderBy numCols idx ) cells


cellsAlive : Model -> Int
cellsAlive model =
    Array.length <| Array.filter (\n -> n == True) model.cells
