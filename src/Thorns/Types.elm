module Thorns.Types exposing
    ( Model
    , Msg(..)
    , Opponent(..)
    , initialModel
    )

import Beings.Beings as Beings
import Dict exposing (Dict)
import Grid
import Thorns.ThornGrid as ThornGrid


type Msg
    = Noop
    | NewRandomIntsAddToPool (List Int)
    | NewRandomIntsAddToPoolAndInitializeGrid (List Int)
    | SetOpponent Beings.FightingCharacter
    | SetOpponentAndPlayerAndInitializeGrid Beings.FightingCharacter Beings.Player
    | DoActivate Int Int
    | MouseOver Int Int
    | MouseOut Int Int
    | InitializeGrid


type Opponent
    = FightingCharacter Beings.FightingCharacter
    | Ochar Beings.OtherCharacter


type alias Model =
    { gridInteractionOptions : Grid.Grid (Maybe ThornGrid.Thorn)
    , currentSegment : List Grid.Coordinate
    , player : Beings.Player
    , opponent : Maybe Opponent
    , interactionHasFinished : Bool
    , pseudoRandomIntsPool : List Int
    , helpStr : Maybe String
    , imgBaseDir : Maybe String
    }


initialModel : Beings.Player -> Maybe Beings.FightingCharacter -> Maybe String -> Model
initialModel player mbFightCharacter imgBaseDir_ =
    { gridInteractionOptions = Grid.initialize (Grid.Size 6 6) Nothing
    , currentSegment = []
    , player = player
    , opponent = mbFightCharacter |> Maybe.map (\x -> FightingCharacter x)
    , interactionHasFinished = False
    , pseudoRandomIntsPool = []
    , helpStr = Nothing
    , imgBaseDir = imgBaseDir_
    }
