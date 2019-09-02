module GameDefinitions.Game1.Game1Definitions exposing (initialModelFunc)

import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameDefinitions.Common exposing (setAllAsUnexplored)
import GameModel
import Grid3 as Grid
import Thorns.Types
import Tile exposing (Tile(..), Visibility(..))


initialPlayer : Player
initialPlayer =
    let
        elem =
            "@"
    in
    Beings.playerCreationFunc elem "You" 10 10 0


initialFightingCharacter : FightingCharacterId -> String -> Int -> Int -> Int -> FightingCharacter
initialFightingCharacter fcharId species x_coord y_coord floorId =
    let
        elem =
            "e" ++ String.fromInt fcharId
    in
    Beings.fightingCharacterCreationFunc elem fcharId ("fightingChar" ++ String.fromInt fcharId) species x_coord y_coord floorId


dimensions : ( Int, Int )
dimensions =
    ( 80, 60 )


initialModelFunc : Maybe String -> ( GameModel.Model, Bool, Bool )
initialModelFunc imgBaseDir_ =
    let
        player =
            initialPlayer

        fightingCharacter =
            initialFightingCharacter 1 "slime" 5 5 theFloorId

        fightingCharacter2 =
            initialFightingCharacter 2 "small_worm" 3 3 theFloorId

        levers =
            Dict.empty

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            Grid.initialize { width = w, height = h, nr_floors = 1 } Tile.NoTileYet

        roomsInfo =
            Just <| GameModel.RoomsInfo [] 20 12 7

        firstExplored =
            setAllAsUnexplored firstMap

        theFloorId =
            0

        randomlyPositionPlayer =
            True

        createRandomMap =
            True
    in
    ( { player = player
      , fightingCharacters =
            Dict.fromList
                [ ( 1, fightingCharacter )
                , ( 2, fightingCharacter2 )
                ]
      , otherCharacters = Dict.empty
      , level = firstMap -- Grid.Grid Tile
      , explored = firstExplored -- Grid.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing imgBaseDir_
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , viewport_topleft_x = 3 -- Int
      , viewport_topleft_y = 3 --Int
      , window_width = 15
      , window_height = 12
      , total_width = Tuple.first dimensions
      , total_height = Tuple.second dimensions
      , currentDisplay = GameModel.DisplayRegularGame
      , displayStatsOverlay = False
      , showBlood = True
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = roomsInfo --  RoomsInfo
      , floorDict = Dict.empty
      , currentFloorId = theFloorId
      , gameCompletionFunc = \fid coords -> False
      , leverModelChangerFuncs = Dict.empty
      , imgBaseDir = imgBaseDir_
      , started = True
      , debugMode = False
      }
    , createRandomMap
    , randomlyPositionPlayer
    )
