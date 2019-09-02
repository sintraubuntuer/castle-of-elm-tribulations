module Thorns.Update exposing
    ( cmdFillRandomIntsPool
    , update
    )

import Beings.Beings as Beings
import Dict exposing (Dict)
import Grid2 as Grid
import Html exposing (Html, a, br, div, h1, h2, h3, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Random
import Thorns.OpponentInteraction as OpponentInteraction
import Thorns.ThornGrid as ThornGrid
import Thorns.Types as Types exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NewRandomIntsAddToPool lints ->
            ( { model
                | pseudoRandomIntsPool = model.pseudoRandomIntsPool ++ lints
              }
            , Cmd.none
            )

        NewRandomIntsAddToPoolAndInitializeGrid lints ->
            { model
                | pseudoRandomIntsPool = model.pseudoRandomIntsPool ++ lints
            }
                |> update InitializeGrid

        SetOpponent opponent ->
            ( { model
                | opponent = Just (Types.FightingCharacter opponent)
                , interactionHasFinished = False
              }
            , Cmd.none
            )

        SetOpponentAndPlayerAndInitializeGrid opponent player ->
            { model
                | opponent = Just (Types.FightingCharacter opponent)
                , player = player
                , interactionHasFinished = False
            }
                |> update InitializeGrid

        MouseOver rownr colnr ->
            let
                rowColStr =
                    String.fromInt rownr ++ " , " ++ String.fromInt colnr

                thorn =
                    Grid.get (Grid.Coordinate colnr rownr) model.gridInteractionOptions |> Maybe.withDefault Nothing

                currentSegment =
                    ThornGrid.findSegment (Grid.Coordinate colnr rownr) model.gridInteractionOptions
                        |> Maybe.withDefault []

                segmentLength =
                    List.length currentSegment
            in
            ( { model
                | currentSegment = currentSegment
                , helpStr = thornToHelpStr thorn segmentLength
              }
            , Cmd.none
            )

        MouseOut rownr colnr ->
            ( { model
                | currentSegment = []
                , helpStr = Nothing
              }
            , Cmd.none
            )

        DoActivate rownr colnr ->
            case model.opponent of
                Nothing ->
                    ( model, Cmd.none )

                Just (Types.Ochar opp) ->
                    ( model, Cmd.none )

                Just (Types.FightingCharacter opponent) ->
                    let
                        coords =
                            Grid.Coordinate colnr rownr

                        previous_grid =
                            model.gridInteractionOptions

                        mbSegment =
                            ThornGrid.findSegment coords model.gridInteractionOptions

                        info_rec =
                            -- { grid = finalGrid , player = newPlayer , opponent = newOpponent , txtmsg = txtmsg }
                            OpponentInteraction.do_activate_and_calc_opponents coords mbSegment model.gridInteractionOptions model.player opponent

                        ( newGrid, newlrands ) =
                            ThornGrid.fall ( info_rec.grid, model.pseudoRandomIntsPool ) info_rec.player
                                |> ThornGrid.checkSegments info_rec.player

                        interactionHasFinished_ =
                            info_rec.player.health <= 0 || info_rec.opponent.health <= 0 || info_rec.opponent.indexOfLight >= info_rec.opponent.indexOfLightMax

                        newModel =
                            { model | gridInteractionOptions = newGrid, currentSegment = [], player = info_rec.player, opponent = Just (Types.FightingCharacter info_rec.opponent), pseudoRandomIntsPool = newlrands, interactionHasFinished = interactionHasFinished_ }
                    in
                    ( newModel, cmdFillRandomIntsPool False newModel )

        InitializeGrid ->
            let
                gridInteractionOptions =
                    model.gridInteractionOptions

                lrandints =
                    model.pseudoRandomIntsPool

                ( newGrid, newlrandints ) =
                    ThornGrid.randomizeGrid gridInteractionOptions lrandints model.player

                newModel =
                    { model | gridInteractionOptions = newGrid, pseudoRandomIntsPool = newlrandints }
            in
            ( newModel, cmdFillRandomIntsPool False newModel )


thornToHelpStr : Maybe ThornGrid.Thorn -> Int -> Maybe String
thornToHelpStr mbthorn segmentLength =
    if segmentLength < 2 then
        Nothing

    else
        case mbthorn of
            Just Beings.COMMON_ATTACK ->
                Just <| "click for a common attack (power of " ++ String.fromInt segmentLength ++ ")"

            Just Beings.ENLIGHTENMENT_SPELL ->
                Just <| "click to throw an enlightenment spell (power of " ++ String.fromInt segmentLength ++ ")"

            Just Beings.OPPONENT_COMMON_ATTACK ->
                Just <| "click for your opponent  common attack (power of " ++ String.fromInt segmentLength ++ ")"

            Just Beings.OPPONENT_ENLIGHTENMENT_SPELL ->
                Just <| "click for your opponent to throw an enlightenment spell (power of " ++ String.fromInt segmentLength ++ ")"

            Nothing ->
                Nothing


cmdFillRandomIntsPool : Bool -> Model -> Cmd Msg
cmdFillRandomIntsPool doInitializeGrid model =
    let
        nrToAdd =
            500 - List.length model.pseudoRandomIntsPool
    in
    if nrToAdd > 0 then
        if doInitializeGrid then
            Random.generate NewRandomIntsAddToPoolAndInitializeGrid (Random.list nrToAdd (Random.int 1 100))

        else
            Random.generate NewRandomIntsAddToPool (Random.list nrToAdd (Random.int 1 100))

    else
        Cmd.none
