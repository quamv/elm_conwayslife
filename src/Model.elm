module Model exposing (..)

import Array exposing (..)
import Time exposing (..)


type Msg
    = TogglePaused
    | ToggleCell Int
    | TickGame
    | AutoTickGame Time.Posix


type GameState
    = Paused
    | Playing


type alias Model =
    { isPaused : Bool
    , cells : Array Bool
    , stepDelay : Int
    , generation : Int
    }


numRows =
    10


numCols =
    10
