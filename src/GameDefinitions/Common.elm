module GameDefinitions.Common exposing (dimensions, gridInitializer, initialEnemy, initialPlayer, initialStateFunc, setAllAsUnexplored)

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameModel
import Grid


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
    GameModel.player elem "You"


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
    GameModel.enemy elem enemyid ("enemy" ++ String.fromInt enemyid)


dimensions : ( Int, Int )
dimensions =
    ( 10, 10 )


initialStateFunc : ( GameModel.State, Bool )
initialStateFunc =
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
            gridInitializer w h

        createRandomMap =
            False
    in
    -- GameModel.State
    ( { player = player
      , enemies =
            Dict.fromList
                [ ( 1, enemy )
                , ( 2, enemy2 )
                ]
      , level = firstMap -- Grid.Grid Tile
      , explored = setAllAsUnexplored firstMap -- Grid.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int
      , y_display_anchor = 3 --Int
      , window_width = 10
      , window_height = 10
      , total_width = Tuple.first dimensions
      , total_height = Tuple.second dimensions
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = Dict.empty
      , currentFloorId = 1
      , started = False
      }
    , createRandomMap
    )


gridInitializer : Int -> Int -> Grid.Grid GameModel.Tile
gridInitializer width_ height_ =
    Grid.initialize { width = width_, height = height_ } GameModel.NoTileYet
