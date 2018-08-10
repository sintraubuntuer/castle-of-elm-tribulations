module Main exposing (..)

--import Generator
--import Generator.Standard
--import MapGen

import Color exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import GameModel
import GameUpdate
import GameView
import Grid
import Html exposing (Html)
import Keyboard
import String
import Text


title : String
title =
    "Chimera"


seed : Int
seed =
    2015


dimensions : ( Int, Int )
dimensions =
    ( 30, 20 )



--( 10, 10 )


initialLevel : Grid.Grid GameModel.Tile
initialLevel =
    let
        toTile c =
            case c of
                ' ' ->
                    GameModel.Floor GameModel.defaultFloorInfo

                '#' ->
                    GameModel.Wall GameModel.defaultWallInfo

                '+' ->
                    GameModel.Door GameModel.defaultDoorInfo

                _ ->
                    GameModel.NoTileYet

        s =
            [ "####################"
            , "#        #         #"
            , "#        #         #"
            , "#                  #"
            , "#        #         #"
            , "#        #         #"
            , "####################"
            ]
    in
    Grid.fromList <| List.map (\x -> List.map toTile <| String.toList x) s


initialExplored : Grid.Grid GameModel.Visibility
initialExplored =
    let
        grid =
            Grid.toList initialLevel
    in
    List.map (\row -> List.map (\_ -> GameModel.Unexplored) row) grid |> Grid.fromList


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
                |> Text.fromString
                |> Text.monospace
                |> Text.color white
                |> centered
    in
    GameModel.player elem "You"


initialEnemy : GameModel.EnemyId -> GameModel.Enemy
initialEnemy enemyid =
    let
        elem =
            ("e" ++ toString enemyid)
                |> Text.fromString
                |> Text.monospace
                |> Text.color white
                |> centered
    in
    GameModel.enemy elem enemyid ("enemy" ++ toString enemyid)


initialState : GameModel.State
initialState =
    let
        player =
            initialPlayer

        enemy =
            initialEnemy 1

        enemy2 =
            initialEnemy 2

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            -- MapGen.randomCave dimensions
            Grid.initialize { width = w, height = h } GameModel.NoTileYet

        firstExplored =
            setAllAsUnexplored firstMap
    in
    GameModel.State
        player
        (Dict.fromList
            [ ( 1, enemy )
            , ( 2, enemy2 )
            ]
        )
        firstMap
        firstExplored
        [ "you enter the dungeon" ]
        []
        3
        3
        12
        12
        (Tuple.first dimensions)
        (Tuple.second dimensions)


subscriptions : GameModel.State -> Sub GameUpdate.Msg
subscriptions model =
    --Keyboard.presses (\code ->  (Char.fromCode code))
    Sub.batch
        [ Keyboard.downs (\kcode -> GameUpdate.KeyDown (fromCode kcode))

        --, Keyboard.ups (\kcode -> KeyUpMsg (fromCode kcode))
        --Keyboard.presses (\kcode -> KeyPress (fromCode kcode))
        --, Time.every (msPerFrame model * Time.millisecond) StepNoKey
        ]


fromCode : Int -> GameModel.Input
fromCode keyCode =
    case keyCode of
        79 ->
            GameModel.Left

        37 ->
            GameModel.Left

        80 ->
            GameModel.Right

        39 ->
            GameModel.Right

        81 ->
            GameModel.Up

        38 ->
            GameModel.Up

        40 ->
            GameModel.Down

        65 ->
            GameModel.Down

        _ ->
            GameModel.Nop


init : ( GameModel.State, Cmd GameUpdate.Msg )
init =
    let
        initState =
            initialState

        dims =
            initState.level.size

        w =
            dims.width

        h =
            dims.height

        gBounds =
            Grid.getGridBoundsToPlacePlayer initState.level
    in
    ( initState
    , Cmd.batch
        ([ GameUpdate.cmdGenFloatsForRandomCave w h
         , GameUpdate.cmdGetRandomPositionedPlayer initState.player gBounds.minX gBounds.maxX gBounds.minY gBounds.maxY
         , GameUpdate.cmdFillRandomIntsPool initState
         , GameUpdate.cmdGenerateRandomInitiativeValue "player" Nothing 1 100
         ]
            ++ (Dict.map (\enid enemy -> GameUpdate.cmdGetRandomPositionedEnemy enemy enid gBounds.minX gBounds.maxX gBounds.minY gBounds.maxY) initState.enemies
                    |> Dict.values
               )
            ++ (Dict.map (\enid enemy -> GameUpdate.cmdGenerateRandomInitiativeValue "enemy" (Just enid) 1 100) initState.enemies
                    |> Dict.values
               )
        )
    )


main : Program Never GameModel.State GameUpdate.Msg
main =
    Html.program
        { init = init
        , view = GameView.view
        , update = GameUpdate.update
        , subscriptions = subscriptions
        }
