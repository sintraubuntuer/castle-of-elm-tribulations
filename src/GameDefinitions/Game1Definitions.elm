module GameDefinitions.Game1Definitions exposing (initialStateFunc)

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


initialPlayer : GameModel.Player
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


initialEnemy : GameModel.EnemyId -> GameModel.Enemy
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
    ( 80, 60 )


initialStateFunc : GameModel.State
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
            Grid.initialize { width = w, height = h } GameModel.NoTileYet

        roomsInfo =
            GameModel.RoomsInfo [] 20 12 7

        firstExplored =
            setAllAsUnexplored firstMap
    in
    -- GameModel.State
    { player = player
    , enemies =
        Dict.fromList
            [ ( 1, enemy )
            , ( 2, enemy2 )
            ]
    , level = firstMap -- Grid.Grid Tile
    , levers = levers --Dict LeverId LeverInfo
    , explored = firstExplored -- Grid.Grid Visibility
    , log = [ "you enter the dungeon" ] --List String
    , pseudoRandomIntsPool = [] -- List Int
    , x_display_anchor = 3 -- Int
    , y_display_anchor = 3 --Int
    , window_width = 15
    , window_height = 15
    , total_width = Tuple.first dimensions
    , total_height = Tuple.second dimensions
    , wallPercentage = Nothing -- Maybe Float
    , roomsInfo = roomsInfo --  RoomsInfo
    }