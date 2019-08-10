module Thorns.Update exposing
    ( cmdFillRandomIntsPool
    , update
    )

import Beings
import Dict exposing (Dict)
import Grid
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
                | previousPseudoRandomIntsPool = model.pseudoRandomIntsPool
                , pseudoRandomIntsPool = model.pseudoRandomIntsPool ++ lints
              }
            , Cmd.none
            )

        NewRandomIntsAddToPoolAndInitializeGrid lints ->
            { model
                | previousPseudoRandomIntsPool = model.pseudoRandomIntsPool
                , pseudoRandomIntsPool = model.pseudoRandomIntsPool ++ lints
            }
                |> update InitializeGrid

        SetOpponent opponent ->
            ( { model
                | opponent = Just (Types.Enemy opponent)
                , interactionHasFinished = False
              }
            , Cmd.none
            )

        SetOpponentAndPlayerAndInitializeGrid opponent player ->
            { model
                | opponent = Just (Types.Enemy opponent)
                , player = player
                , interactionHasFinished = False
            }
                |> update InitializeGrid

        MouseOver rownr colnr ->
            let
                rowColStr =
                    String.fromInt rownr ++ " , " ++ String.fromInt colnr

                currentSegment =
                    ThornGrid.findSegment (Grid.Coordinate colnr rownr) model.gridInteractionOptions
                        |> Maybe.withDefault []
            in
            ( { model | currentSegment = currentSegment }, Cmd.none )

        MouseOut rownr colnr ->
            ( { model | currentSegment = [] }, Cmd.none )

        DoActivate rownr colnr ->
            let
                _ =
                    Debug.log "DoActivate called with model.opponent of " model.opponent
            in
            case model.opponent of
                Nothing ->
                    ( model, Cmd.none )

                Just (Types.Ochar opp) ->
                    ( model, Cmd.none )

                Just (Types.Enemy opponent) ->
                    let
                        _ =
                            Debug.log ("DoActivate called with rownr colnr " ++ String.fromInt rownr) colnr

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
                            info_rec.opponent.health <= 0

                        newModel =
                            { model | gridInteractionOptions = newGrid, previousGrid = Just previous_grid, currentSegment = [], player = info_rec.player, opponent = Just (Types.Enemy info_rec.opponent), pseudoRandomIntsPool = newlrands, interactionHasFinished = interactionHasFinished_ }

                        _ =
                            Debug.log "message : " info_rec.txtmsg
                    in
                    ( newModel, cmdFillRandomIntsPool False newModel )

        UndoLastInteraction ->
            let
                modelNewGrid =
                    case model.previousGrid of
                        Just pgrid ->
                            { model | gridInteractionOptions = pgrid, previousGrid = Nothing }

                        Nothing ->
                            model

                modelNewGridAndRandoms =
                    if model.previousPseudoRandomIntsPool /= [] then
                        { modelNewGrid | pseudoRandomIntsPool = model.previousPseudoRandomIntsPool, previousPseudoRandomIntsPool = [] }

                    else
                        modelNewGrid
            in
            ( modelNewGridAndRandoms, Cmd.none )

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