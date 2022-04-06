module GameSeeds exposing (..)

import Array exposing (..)


emptyBoard rows cols =
    Array.repeat (rows * cols) False


middlerows rows cols =
    Array.fromList <|
        List.repeat ((rows // 2) * cols) False
            ++ List.repeat cols True
            ++ List.repeat cols True
            ++ List.repeat cols True
            ++ List.repeat (((rows // 2) - 3) * cols) False
