module Main exposing (main)

import Browser
import Browser.Events
    exposing
        ( onKeyDown
        )
import Collage.Text as Text
import Dict exposing (Dict)
import GameDefinitions.Common
import GameDefinitions.Game1.Game1Definitions
import GameDefinitions.Game2.Game2Definitions
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


subscriptions : GameModel.Model -> Sub GameUpdate.Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map (\kCode -> GameUpdate.KeyDown (fromCode kCode)) keyCode)
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

        49 ->
            GameModel.FloorDown

        50 ->
            GameModel.FloorUp

        85 ->
            GameModel.PickUpItem

        73 ->
            GameModel.ViewInventory

        _ ->
            GameModel.Nop


type alias Flags =
    {}


init : Flags -> ( GameModel.Model, Cmd GameUpdate.Msg )
init flags =
    let
        ( initModel, createRandomMap ) =
            GameDefinitions.Common.initialModelFunc

        gBounds =
            Grid.getGridBoundsToPlacePlayer initModel.level
    in
    ( initModel, GameUpdate.cmdFillRandomIntsPool initModel )


main : Program Flags GameModel.Model GameUpdate.Msg
main =
    Browser.element
        { init = init
        , view = GameView.view
        , update = GameUpdate.update
        , subscriptions = subscriptions
        }
