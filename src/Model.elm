module Model exposing (..)

import Time exposing (..)
import Array exposing (..)

type Msg = TogglePaused | ToggleCell Int | TickGame | AutoTickGame Time
type GameState = Paused | Playing

type alias Model = {
    isPaused : Bool
    ,cells : Array Bool
    ,stepDelay : Int
    ,generation : Int
    }

numRows = 10
numCols = 10
