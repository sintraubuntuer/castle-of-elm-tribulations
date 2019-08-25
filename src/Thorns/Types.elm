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
    | UndoLastInteraction
    | InitializeGrid


type Opponent
    = FightingCharacter Beings.FightingCharacter
    | Ochar Beings.OtherCharacter


type alias Model =
    { gridInteractionOptions : Grid.Grid (Maybe ThornGrid.Thorn)
    , currentSegment : List Grid.Coordinate
    , previousGrid : Maybe (Grid.Grid (Maybe ThornGrid.Thorn))
    , player : Beings.Player
    , opponent : Maybe Opponent
    , interactionHasFinished : Bool
    , pseudoRandomIntsPool : List Int
    , helpStr : Maybe String
    , previousPseudoRandomIntsPool : List Int
    }


initialModel : Beings.Player -> Maybe Beings.FightingCharacter -> Model
initialModel player mbFightCharacter =
    { gridInteractionOptions = Grid.initialize (Grid.Size 6 6) Nothing
    , currentSegment = []
    , previousGrid = Nothing
    , player = player
    , opponent = mbFightCharacter |> Maybe.map (\x -> FightingCharacter x)
    , interactionHasFinished = False
    , pseudoRandomIntsPool = []
    , helpStr = Nothing
    , previousPseudoRandomIntsPool = []
    }
