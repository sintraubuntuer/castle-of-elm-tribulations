module Main exposing (main)

{- }
   ( Flags
   , dimensions
   , fromCode
   , init
   , initialEnemy
   , initialExplored
   , initialLevel
   , initialPlayer
   , initialStateFunc
   , main
   , seed
   , setAllAsUnexplored
   , subscriptions
   , title
   )
-}
--import Generator
--import Generator.Standard
--import MapGen
--import Keyboard
--import Element exposing (..)
--import Color exposing (..)

import Browser
import Browser.Events
    exposing
        ( onKeyDown
        )
import Collage.Text as Text
import Dict exposing (Dict)
import GameDefinitions.Game1Definitions
import GameDefinitions.Game2Definitions
import GameModel
import GameUpdate
import GameView
import Grid
import Html exposing (Html)
import Html.Events exposing (keyCode)
import Json.Decode as Decode exposing (Value)
import String


title : String
title =
    "Chimera"


seed : Int
seed =
    2015


dimensions : ( Int, Int )
dimensions =
    ( 80, 60 )



--( 110, 70 )
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


subscriptions : GameModel.State -> Sub GameUpdate.Msg
subscriptions model =
    --Keyboard.presses (\code ->  (Char.fromCode code))
    Sub.batch
        [ -- Keyboard.downs (\kcode -> GameUpdate.KeyDown (fromCode kcode))
          onKeyDown (Decode.map (\kCode -> GameUpdate.KeyDown (fromCode kCode)) keyCode)

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


type alias Flags =
    {}


init : Flags -> ( GameModel.State, Cmd GameUpdate.Msg )
init flags =
    let
        initState =
            --GameDefinitions.Game1Definitions.initialStateFunc
            GameDefinitions.Game2Definitions.initialStateFunc

        dims =
            initState.level.size

        w =
            dims.width

        h =
            dims.height

        gBounds =
            Grid.getGridBoundsToPlacePlayer initState.level

        createRandomMap : Bool
        createRandomMap =
            False
    in
    ( initState
    , Cmd.batch
        ([ -- GameUpdate.cmdGenFloatsForRandomCave w h
           --, GameUpdate.cmdFillRandomIntsPool initState
           if createRandomMap then
            GameUpdate.cmdFillRandomIntsPoolAndGenerateRandomMap initState

           else
            GameUpdate.cmdFillRandomIntsPool initState
         , GameUpdate.cmdGenerateRandomInitiativeValue "player" Nothing 1 100
         , GameUpdate.cmdGetRandomPositionedPlayer initState.player gBounds.minX gBounds.maxX gBounds.minY gBounds.maxY
         ]
            ++ (Dict.map (\enid enemy -> GameUpdate.cmdGetRandomPositionedEnemy enemy enid gBounds.minX gBounds.maxX gBounds.minY gBounds.maxY) initState.enemies
                    |> Dict.values
               )
            ++ (Dict.map (\enid enemy -> GameUpdate.cmdGenerateRandomInitiativeValue "enemy" (Just enid) 1 100) initState.enemies
                    |> Dict.values
               )
        )
    )


main : Program Flags GameModel.State GameUpdate.Msg
main =
    Browser.element
        { init = init
        , view = GameView.view
        , update = GameUpdate.update
        , subscriptions = subscriptions
        }
