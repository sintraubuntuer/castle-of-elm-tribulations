module GameDefinitions.Game1Definitions exposing (initialModelFunc)

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameModel
import Grid
import Thorns.Types


setAllAsUnexplored : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Visibility
setAllAsUnexplored level =
    let
        grid =
            Grid.toList level
    in
    List.map (\row -> List.map (\_ -> GameModel.Unexplored) row) grid |> Grid.fromList


initialPlayer : Player
initialPlayer =
    let
        elem =
            "@"

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    Beings.playerCreationFunc elem "You"


initialEnemy : EnemyId -> Enemy
initialEnemy enemyid =
    let
        elem =
            "e" ++ String.fromInt enemyid

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    Beings.enemyCreationFunc elem enemyid ("enemy" ++ String.fromInt enemyid)


dimensions : ( Int, Int )
dimensions =
    ( 80, 60 )


initialModelFunc : ( GameModel.Model, Bool, Bool )
initialModelFunc =
    let
        player =
            initialPlayer

        enemy =
            initialEnemy 1

        enemy2 =
            initialEnemy 2

        levers =
            Dict.empty

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            -- MapGen.randomCave dimensions
            Grid.initialize { width = w, height = h } GameModel.NoTileYet

        roomsInfo =
            Just <| GameModel.RoomsInfo [] 20 12 7

        firstExplored =
            setAllAsUnexplored firstMap

        randomlyPositionPlayer =
            True

        createRandomMap =
            True
    in
    -- GameModel.Model
    ( { player = player
      , enemies =
            Dict.fromList
                [ ( 1, enemy )
                , ( 2, enemy2 )
                ]
      , otherCharacters = Dict.empty
      , level = firstMap -- Grid.Grid Tile

      --, levers = levers --Dict LeverId LeverInfo
      , explored = firstExplored -- Grid.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing
      , gameOfThornsModeisOn = False
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int
      , y_display_anchor = 3 --Int
      , window_width = 15
      , window_height = 12
      , total_width = Tuple.first dimensions
      , total_height = Tuple.second dimensions
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = roomsInfo --  RoomsInfo
      , floorDict = Dict.empty
      , currentFloorId = 1
      , started = True
      }
    , createRandomMap
    , randomlyPositionPlayer
    )
