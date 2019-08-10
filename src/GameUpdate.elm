module GameUpdate exposing
    ( Msg(..)
    , attack
    , attackIfClose
    , checkAndAlterDisplayAnchorIfNecessary
    , cleanup
    , cmdFillRandomIntsPool
    , cmdFillRandomIntsPoolAndGenerateRandomMap
    , cmdGenFloatsForRandomCave
    , cmdGenerateRandomInitiativeValue
    , cmdGetRandomPositionedEnemy
    , cmdGetRandomPositionedPlayer
    , enemy_AI
    , getRandIntPair
    , getTailWithDefaultEmptyList
    , increseNrOfEnemyMovesInCurrentTurn
    , log
    , move
    , randIntList
    , randIntPairsList
    , resetEnemyMovesCurrentTurn
    , reveal
    , turnNeighbourWallCellstoAshes
    , update
    )

--import Generator
--import Collage

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameDefinitions.Game1Definitions
import GameDefinitions.Game2Definitions
import GameModel
import Grid
import Item exposing (Item(..), KeyInfo)
import MapGen
import Random
import Thorns.ThornGrid as ThornGrid
import Thorns.Types
import Thorns.Update as ThornsUpdate


log : String -> GameModel.Model -> GameModel.Model
log s model =
    { model | log = s :: model.log }


getTailWithDefaultEmptyList : List a -> List a
getTailWithDefaultEmptyList la =
    List.tail la
        |> Maybe.withDefault []


type alias FloorId =
    Int


type Msg
    = Noop
    | StartGameNr Int
    | KeyDown GameModel.Input
    | TryAddToPlayerInventory
    | TryShiftPlayerPosition ( Int, Int )
    | StartOpponentInteraction Enemy
    | ChangeFloorTo FloorId ( Int, Int )
    | NewRandomPointToPlacePlayer ( Int, Int )
    | NewRandomPointToPlaceEnemy EnemyId ( Int, Int )
    | NewRandomFloatsForGenCave (List Float)
    | RandomInitiativeValue String (Maybe EnemyId) Int
    | NewRandomIntsAddToPool (List Int)
    | NewRandomIntsAddToPoolAndGenerateRandomMap (List Int)
    | ThornsMsg Thorns.Types.Msg


update : Msg -> GameModel.Model -> ( GameModel.Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        ThornsMsg tmsg ->
            let
                ( newThornsModel, thorns_cmds ) =
                    ThornsUpdate.update tmsg model.gameOfThornsModel

                newModel =
                    if newThornsModel.interactionHasFinished then
                        let
                            ( newEnemies, newOtherCharacters, newPlayer ) =
                                case newThornsModel.opponent of
                                    Just (Thorns.Types.Enemy erec) ->
                                        ( Dict.update erec.id (\_ -> Just erec) model.enemies
                                        , model.otherCharacters
                                        , newThornsModel.player
                                        )

                                    Just (Thorns.Types.Ochar orec) ->
                                        ( model.enemies
                                        , Dict.update orec.id (\_ -> Just orec) model.otherCharacters
                                        , newThornsModel.player
                                        )

                                    Nothing ->
                                        ( model.enemies, model.otherCharacters, model.player )
                        in
                        { model
                            | enemies = newEnemies
                            , otherCharacters = newOtherCharacters
                            , player = newPlayer
                            , gameOfThornsModel = newThornsModel
                            , gameOfThornsModeisOn = model.gameOfThornsModeisOn && not newThornsModel.interactionHasFinished
                            , listeningToKeyInput = True
                        }

                    else
                        { model | gameOfThornsModel = newThornsModel, gameOfThornsModeisOn = model.gameOfThornsModeisOn && not newThornsModel.interactionHasFinished }
            in
            ( newModel, Cmd.map ThornsMsg thorns_cmds )

        StartGameNr nr ->
            let
                ( initModel, createRandomMap, randomlyPositionPlayer ) =
                    case nr of
                        1 ->
                            GameDefinitions.Game1Definitions.initialModelFunc

                        2 ->
                            GameDefinitions.Game2Definitions.initialModelFunc

                        _ ->
                            ( model, False, False )

                test =
                    ThornGrid.thornToString Beings.CHICANE_ATTACK

                gBounds =
                    Grid.getGridBoundsToPlacePlayer initModel.level
            in
            ( initModel |> position_display_anchor_in_order_to_center_player
            , Cmd.batch
                ([ if createRandomMap then
                    cmdFillRandomIntsPoolAndGenerateRandomMap initModel
                    --|> Debug.log "trying to create a random map"

                   else
                    cmdFillRandomIntsPool initModel
                 , cmdGenerateRandomInitiativeValue "player" Nothing 1 100
                 , if randomlyPositionPlayer then
                    cmdGetRandomPositionedPlayer initModel.player gBounds.minX gBounds.maxX gBounds.minY gBounds.maxY

                   else
                    Cmd.none
                 , Cmd.map ThornsMsg (ThornsUpdate.cmdFillRandomIntsPool True initModel.gameOfThornsModel)
                 ]
                    ++ (Dict.map (\enid enemy -> cmdGetRandomPositionedEnemy enemy enid gBounds.minX gBounds.maxX gBounds.minY gBounds.maxY) initModel.enemies
                            |> Dict.values
                       )
                    ++ (Dict.map (\enid enemy -> cmdGenerateRandomInitiativeValue "enemy" (Just enid) 1 100) initModel.enemies
                            |> Dict.values
                       )
                )
            )

        KeyDown input ->
            if not model.listeningToKeyInput then
                let
                    _ =
                        Debug.log "currently not listening to key input : " (not model.listeningToKeyInput)
                in
                ( model, Cmd.none )

            else
                case input of
                    GameModel.Up ->
                        update (TryShiftPlayerPosition ( 0, 0 - 1 )) model

                    GameModel.Down ->
                        update (TryShiftPlayerPosition ( 0, 0 + 1 )) model

                    GameModel.Left ->
                        update (TryShiftPlayerPosition ( 0 - 1, 0 )) model

                    GameModel.Right ->
                        update (TryShiftPlayerPosition ( 0 + 1, 0 )) model

                    GameModel.PickUpItem ->
                        update TryAddToPlayerInventory model

                    GameModel.ViewInventory ->
                        -- for the time being
                        let
                            _ =
                                Debug.log "player inventory : " (Dict.keys model.player.inventory)
                        in
                        ( model, Cmd.none )

                    GameModel.FloorUp ->
                        update (ChangeFloorTo (model.currentFloorId + 1) ( model.player.location.x, model.player.location.y )) model

                    GameModel.FloorDown ->
                        update (ChangeFloorTo (model.currentFloorId - 1) ( model.player.location.x, model.player.location.y )) model

                    GameModel.Nop ->
                        ( model, Cmd.none )

        TryAddToPlayerInventory ->
            let
                player_ =
                    model.player

                pcoords =
                    model.player.location

                --checkIfTheresAnItemLocatedAt pcoords
                ( updatedInventory, newGrid ) =
                    case Grid.get pcoords model.level of
                        Just (GameModel.Floor floorinfo) ->
                            case floorinfo.item of
                                Just item ->
                                    ( Dict.update (GameModel.itemToString item) (\_ -> Just item) model.player.inventory
                                    , Grid.set pcoords (GameModel.Floor { floorinfo | item = Nothing }) model.level
                                    )

                                _ ->
                                    ( model.player.inventory, model.level )

                        _ ->
                            ( model.player.inventory, model.level )

                newPlayer =
                    { player_ | inventory = updatedInventory }
            in
            ( { model | player = newPlayer, level = newGrid }, Cmd.none )

        TryShiftPlayerPosition shiftTuple ->
            let
                player =
                    model.player

                { x, y } =
                    player.location

                x_ =
                    Tuple.first shiftTuple

                y_ =
                    Tuple.second shiftTuple

                ( x2, y2 ) =
                    ( x + x_, y + y_ )

                newModel =
                    --GameModel.location x2 y2
                    case Grid.get (GameModel.location x2 y2) model.level of
                        Just (GameModel.Lever leverinfo) ->
                            if leverinfo.isUp then
                                model
                                --|> Debug.log " you just interacted with an up lever "

                            else
                                { model | level = Grid.set (GameModel.location x2 y2) (GameModel.Lever { leverinfo | isUp = True }) model.level }
                                    |> turnNeighbourWallCellstoAshes (GameModel.location x2 y2)

                        --  |> Debug.log " you just interacted with a down lever "
                        _ ->
                            model

                mbEnemy =
                    case Dict.filter (\enemyid enemy -> enemy.location == GameModel.location x2 y2) model.enemies |> Dict.values of
                        [] ->
                            Nothing

                        enemy :: es ->
                            Just enemy

                newModel2 =
                    case mbEnemy of
                        Just enemy ->
                            newModel

                        Nothing ->
                            if x_ /= 0 || y_ /= 0 then
                                { newModel | player = move ( x_, y_ ) newModel newModel.player }
                                    |> checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor
                                    --|> checkIfPlayerStandsOnStairsAndMoveToNewFloor
                                    |> checkAndAlterDisplayAnchorIfNecessary

                            else
                                newModel
            in
            case mbEnemy of
                Just enemy ->
                    update (StartOpponentInteraction enemy) newModel2

                Nothing ->
                    let
                        newModel3 =
                            newModel2 |> cleanup |> resetEnemyMovesCurrentTurn |> enemy_AI |> reveal
                    in
                    ( newModel3, cmdFillRandomIntsPool newModel3 )

        StartOpponentInteraction enemy ->
            let
                ( newThornsModel, thornsCmd ) =
                    ThornsUpdate.update (Thorns.Types.SetOpponentAndPlayerAndInitializeGrid enemy model.player) model.gameOfThornsModel

                -- ThornsUpdate.update (Thorns.Types.SetOpponent enemy)
                {- }
                   attackOutput =
                       attack model.player enemy model.pseudoRandomIntsPool

                   newModel =
                       log attackOutput.textMsg
                           { model
                               | player = attackOutput.dudeA
                               , enemies = Dict.insert enemy.id attackOutput.dudeB model.enemies --enemy_ :: getTailWithDefaultEmptyList model.enemies
                               , pseudoRandomIntsPool = attackOutput.randInts
                           }
                -}
                newModel =
                    { model
                        | gameOfThornsModeisOn = True
                        , gameOfThornsModel = newThornsModel
                        , listeningToKeyInput = False
                    }

                newModel_after_cleanup =
                    --newModel |> cleanup |> resetEnemyMovesCurrentTurn |> enemy_AI |> reveal
                    newModel |> cleanup |> reveal
            in
            ( newModel_after_cleanup
            , Cmd.batch
                [ cmdFillRandomIntsPool newModel_after_cleanup
                , Cmd.map ThornsMsg thornsCmd
                ]
            )

        ChangeFloorTo floorId locTuple ->
            let
                newModel =
                    changeFloorTo model floorId locTuple
            in
            ( newModel, Cmd.none )

        NewRandomPointToPlacePlayer tupPosition ->
            let
                oldPlayer =
                    model.player

                newLocation =
                    GameModel.location (Tuple.first tupPosition) (Tuple.second tupPosition)

                gridBounds =
                    Grid.getGridBoundsToPlacePlayer model.level

                --|> Debug.log "grid bounds are : "
                newPlayer =
                    { oldPlayer | location = newLocation, placed = True }

                --_ =
                --    Debug.log "new player position is walkable = " (GameModel.isModelTileWalkable newLocation model)
            in
            case GameModel.isModelTileWalkable newLocation model of
                True ->
                    ( { model
                        | player = newPlayer
                        , x_display_anchor = max 0 (newLocation.x - round (toFloat model.window_width / 2.0))
                        , y_display_anchor = max 0 (newLocation.y - round (toFloat model.window_height / 2))
                      }
                        |> reveal
                    , Cmd.none
                    )

                False ->
                    ( model, cmdGetRandomPositionedPlayer { oldPlayer | placed = False } gridBounds.minX gridBounds.maxX gridBounds.minY gridBounds.maxY )

        NewRandomPointToPlaceEnemy enemyId tupPosition ->
            let
                mbActualEnemy =
                    Dict.get enemyId model.enemies

                newLocation =
                    GameModel.location (Tuple.first tupPosition) (Tuple.second tupPosition)

                gridBounds =
                    Grid.getGridBoundsToPlaceEnemy model.level

                mbNewEnemy =
                    case mbActualEnemy of
                        Just actualEnemy ->
                            Just { actualEnemy | location = newLocation, placed = True }

                        Nothing ->
                            Nothing
            in
            case mbActualEnemy of
                Nothing ->
                    ( model, Cmd.none )

                Just actualEnemy ->
                    case GameModel.isModelTileWalkable newLocation model of
                        True ->
                            ( { model | enemies = GameModel.placeExistingEnemy enemyId newLocation model.enemies }, Cmd.none )

                        False ->
                            --( { model | player = newPlayer }, Cmd.none )
                            ( model, cmdGetRandomPositionedEnemy actualEnemy enemyId gridBounds.minX gridBounds.maxX gridBounds.minY gridBounds.maxY )

        --randomlyPlaceExistingEnemies : List ( Location, EnemyId ) -> Model -> Model
        NewRandomFloatsForGenCave lfloats ->
            let
                theSize =
                    model.level.size

                newGrid =
                    MapGen.randomCave ( theSize.width, theSize.height ) lfloats
            in
            ( { model | level = newGrid }, Cmd.none )

        RandomInitiativeValue strCharacter mbCharacterId intval ->
            if strCharacter == "player" then
                let
                    oldPlayer =
                        model.player

                    newPlayer =
                        { oldPlayer | initiative = intval }
                in
                ( { model | player = newPlayer }, Cmd.none )

            else if strCharacter == "enemy" then
                let
                    newModel =
                        GameModel.mbUpdateEnemyInitiativeByMbEnemyId intval mbCharacterId model
                in
                ( newModel, Cmd.none )

            else
                ( model, Cmd.none )

        NewRandomIntsAddToPool lints ->
            ( { model | pseudoRandomIntsPool = lints ++ model.pseudoRandomIntsPool }
            , Cmd.none
            )

        NewRandomIntsAddToPoolAndGenerateRandomMap lints ->
            let
                maxNrOfRooms =
                    model.roomsInfo |> Maybe.map .maxNrOfRooms |> Maybe.withDefault 0

                maxRoomSize =
                    model.roomsInfo |> Maybe.map .maxRoomSize |> Maybe.withDefault 0

                minRoomSize =
                    model.roomsInfo |> Maybe.map .minRoomSize |> Maybe.withDefault 0

                --( newGrid, lrectangles, ltunnelrectangles, unused_prand_lints ) =
                genOutputRecord =
                    --{ tileGrid = gridAfterInstallLevers, lroomRectangles = lroomrectangles, ltunnelRectangles = ltunnelrectangles, unusedRandoms = lremainingrandints }
                    MapGen.randomMapGeneratorWithRooms model.total_width model.total_height maxNrOfRooms maxRoomSize minRoomSize lints model.level

                gridAsList =
                    Grid.toList genOutputRecord.tileGrid |> List.concatMap identity

                wallPercentage =
                    getWallPercentage gridAsList

                newmodel =
                    { model
                        | level = genOutputRecord.tileGrid
                        , pseudoRandomIntsPool = genOutputRecord.unusedRandoms
                        , wallPercentage = Just wallPercentage
                    }
            in
            ( newmodel, cmdFillRandomIntsPool newmodel )


isPlayerStandingOnStairs : GameModel.Model -> Bool
isPlayerStandingOnStairs model =
    --False
    let
        mbTile =
            Grid.get model.player.location model.level
    in
    case mbTile of
        Just (GameModel.Stairs sinfo) ->
            True

        _ ->
            False


checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor : GameModel.Model -> GameModel.Model
checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor model =
    let
        mbTile =
            Grid.get model.player.location model.level
    in
    case mbTile of
        Just (GameModel.Stairs sinfo) ->
            let
                mbDestinationCoordsTuple =
                    searchFloorForStairsId sinfo.toFloorId sinfo.toStairsId model
            in
            case mbDestinationCoordsTuple of
                Just ( newX, newY ) ->
                    let
                        _ =
                            Debug.log "going to call changeFloorTo with coords " ( newX, newY )
                    in
                    changeFloorTo model sinfo.toFloorId ( newX + Tuple.first sinfo.shift, newY + Tuple.second sinfo.shift )

                Nothing ->
                    model

        Just (GameModel.Hole hinfo) ->
            let
                lfloorIds =
                    model.floorDict |> Dict.filter (\floorId v -> floorId < model.currentFloorId) |> Dict.keys

                mbDestinationFloorAndCoordsTuple =
                    List.map (\floorId -> searchFloorForTargetId floorId hinfo.target_id model) lfloorIds
                        |> List.filterMap (\x -> x)
                        |> List.head
            in
            case mbDestinationFloorAndCoordsTuple of
                Just ( floorId, newX, newY ) ->
                    let
                        _ =
                            Debug.log "Went through a hole , going to call changeFloorTo with coords " ( newX, newY )
                    in
                    changeFloorTo model floorId ( newX, newY )

                Nothing ->
                    model

        Just (GameModel.Wall wallinfo) ->
            case wallinfo.mbTeleporterObject of
                Nothing ->
                    model

                Just teleporter ->
                    let
                        sameFloorId =
                            model.currentFloorId

                        lOtherFloorIds =
                            model.floorDict |> Dict.filter (\floorId v -> floorId /= model.currentFloorId) |> Dict.keys

                        mbDestinationFloorAndCoordsTuple =
                            case searchFloorForTeleporterId sameFloorId teleporter.target_id model of
                                Just ( floorid, xcoord, ycoord ) ->
                                    Just ( floorid, xcoord + Tuple.first teleporter.shift, ycoord + Tuple.second teleporter.shift )

                                Nothing ->
                                    List.map (\floorId -> searchFloorForTeleporterId floorId teleporter.target_id model) lOtherFloorIds
                                        |> List.filterMap (\x -> x)
                                        |> List.head
                                        |> Maybe.map (\( floorid, xcoord, ycoord ) -> ( floorid, xcoord + Tuple.first teleporter.shift, ycoord + Tuple.second teleporter.shift ))
                    in
                    case mbDestinationFloorAndCoordsTuple of
                        Just ( floorId, newX, newY ) ->
                            let
                                _ =
                                    Debug.log "Went through a teleporter , going to call changeFloorTo with floorId and coords " ( floorId, newX, newY )
                            in
                            changeFloorTo model floorId ( newX, newY )

                        Nothing ->
                            model

        _ ->
            model


searchFloorForTeleporterId : Int -> Int -> GameModel.Model -> Maybe ( Int, Int, Int )
searchFloorForTeleporterId fid target_id_ state =
    let
        mbFloorGrid =
            Dict.get fid state.floorDict

        getlcoords fgrid =
            Grid.toCoordinates fgrid

        checkCoords coords_ fgrid =
            case Grid.get coords_ fgrid of
                Just (GameModel.Wall wallinfo) ->
                    case wallinfo.mbTeleporterObject of
                        Just ateleporter ->
                            ateleporter.teleporter_id == target_id_

                        Nothing ->
                            False

                _ ->
                    False

        _ =
            Debug.log ("searching for teleporter with targetId " ++ String.fromInt target_id_ ++ " in floor ") fid
    in
    case mbFloorGrid of
        Just floorGrid ->
            List.filter (\coords -> checkCoords coords floorGrid.level) (getlcoords floorGrid.level)
                |> List.head
                |> Maybe.map (\rec -> ( fid, rec.x, rec.y ))
                |> Debug.log "coords of targetId are : "

        Nothing ->
            Nothing


searchFloorForTargetId : Int -> Int -> GameModel.Model -> Maybe ( Int, Int, Int )
searchFloorForTargetId fid target_id state =
    let
        mbFloorGrid =
            Dict.get fid state.floorDict

        getlcoords fgrid =
            Grid.toCoordinates fgrid

        checkCoords coords_ fgrid =
            case Grid.get coords_ fgrid of
                Just (GameModel.Floor finfo) ->
                    case finfo.floorDrawing of
                        Just (GameModel.LandingTargetDrawing tid) ->
                            target_id == tid

                        _ ->
                            False

                _ ->
                    False

        _ =
            Debug.log ("searching for targetId " ++ String.fromInt target_id ++ " in floor ") fid
    in
    case mbFloorGrid of
        Just floorGrid ->
            List.filter (\coords -> checkCoords coords floorGrid.level) (getlcoords floorGrid.level)
                |> List.head
                |> Maybe.map (\rec -> ( fid, rec.x, rec.y ))
                |> Debug.log "coords of targetId are : "

        Nothing ->
            Nothing


searchFloorForStairsId : Int -> Int -> GameModel.Model -> Maybe ( Int, Int )
searchFloorForStairsId floorId stairsId state =
    let
        mbFloorGrid =
            Dict.get floorId state.floorDict

        getlcoords fgrid =
            Grid.toCoordinates fgrid

        checkCoords coords_ fgrid =
            case Grid.get coords_ fgrid of
                Just (GameModel.Stairs sinfo) ->
                    sinfo.stairsId == stairsId

                _ ->
                    False

        _ =
            Debug.log ("searching for stairsId " ++ String.fromInt stairsId ++ " in floor ") floorId
    in
    case mbFloorGrid of
        Just floorGrid ->
            List.filter (\coords -> checkCoords coords floorGrid.level) (getlcoords floorGrid.level)
                |> List.head
                |> Maybe.map (\rec -> ( rec.x, rec.y ))
                |> Debug.log "coords of stairs are : "

        Nothing ->
            Nothing


changeFloorTo : GameModel.Model -> Int -> ( Int, Int ) -> GameModel.Model
changeFloorTo state floorId locTuple =
    let
        _ =
            Debug.log ("changeFloorTo was called with floorId " ++ String.fromInt floorId ++ " and coords ") locTuple

        newModel =
            if state.currentFloorId == floorId then
                state

            else
                let
                    currentFloorInfo =
                        GameModel.getCurrentFloorInfoToStore state

                    newStore =
                        Dict.update state.currentFloorId (\_ -> Just currentFloorInfo) state.floorDict

                    newCurrentFloor =
                        Dict.get floorId state.floorDict
                in
                case newCurrentFloor of
                    Just cFloor ->
                        { state
                            | level = cFloor.level
                            , explored = cFloor.explored
                            , window_width = cFloor.window_width
                            , window_height = cFloor.window_height
                            , total_width = cFloor.total_width
                            , total_height = cFloor.total_height
                            , floorDict = newStore
                            , currentFloorId = floorId
                        }

                    Nothing ->
                        { state | floorDict = newStore }

        _ =
            Debug.log "newModel currentFloorId is " newModel.currentFloorId

        delta_x =
            Tuple.first locTuple - newModel.player.location.x

        delta_y =
            Tuple.second locTuple - newModel.player.location.y

        player_ =
            move ( delta_x, delta_y ) newModel newModel.player
    in
    { newModel
        | player = player_
        , x_display_anchor = max 0 (Tuple.first locTuple - round (toFloat newModel.window_width / 2.0))
        , y_display_anchor = max 0 (Tuple.second locTuple - round (toFloat newModel.window_height / 2))
    }
        |> reveal


position_display_anchor_in_order_to_center_player : GameModel.Model -> GameModel.Model
position_display_anchor_in_order_to_center_player state =
    { state
        | x_display_anchor = max 0 (state.player.location.x - round (toFloat state.window_width / 2.0))
        , y_display_anchor = max 0 (state.player.location.y - round (toFloat state.window_height / 2))
    }
        |> reveal


turnNeighbourWallCellstoAshes : Grid.Coordinate -> GameModel.Model -> GameModel.Model
turnNeighbourWallCellstoAshes { x, y } state =
    let
        upCell =
            GameModel.location x (y - 1)

        downCell =
            GameModel.location x (y + 1)

        leftCell =
            GameModel.location (x - 1) y

        rightCell =
            GameModel.location (x + 1) y

        convertCellsFunc cellCoords thestate =
            case Grid.get cellCoords thestate.level of
                Just (GameModel.Wall wallinfo) ->
                    let
                        floorinfo =
                            GameModel.defaultFloorInfo
                    in
                    { thestate | level = Grid.set cellCoords (GameModel.Floor { floorinfo | item = Just Ash }) thestate.level }
                        |> turnNeighbourWallCellstoAshes cellCoords

                _ ->
                    thestate
    in
    convertCellsFunc upCell state
        |> convertCellsFunc downCell
        |> convertCellsFunc leftCell
        |> convertCellsFunc rightCell
        |> updateWallPercentageValue


updateWallPercentageValue : GameModel.Model -> GameModel.Model
updateWallPercentageValue state =
    let
        thegrid =
            state.level

        gridAsList =
            Grid.toList thegrid |> List.concatMap identity

        wallPercentage =
            getWallPercentage gridAsList
    in
    { state | wallPercentage = Just wallPercentage }


getWallPercentage : List GameModel.Tile -> Float
getWallPercentage gridAsList =
    let
        getCountTuple : GameModel.Tile -> ( Int, Int ) -> ( Int, Int )
        getCountTuple elem ( acc1, acc2 ) =
            case elem of
                GameModel.Wall _ ->
                    ( acc1 + 1, acc2 )

                GameModel.Floor _ ->
                    ( acc1, acc2 + 1 )

                _ ->
                    ( acc1, acc2 )
    in
    gridAsList
        |> List.foldl (\el ( ac1, ac2 ) -> getCountTuple el ( ac1, ac2 )) ( 0, 0 )
        |> (\tup -> (Tuple.first tup |> toFloat) / (Tuple.second tup |> toFloat))


checkAndAlterDisplayAnchorIfNecessary : GameModel.Model -> GameModel.Model
checkAndAlterDisplayAnchorIfNecessary state =
    let
        p_x_dist =
            5

        newXanchor =
            if state.player.location.x <= state.x_display_anchor then
                max 0 (state.x_display_anchor - (state.window_width - p_x_dist))

            else if state.player.location.x >= (state.x_display_anchor + (state.window_width - 1)) then
                --min (state.x_display_anchor + (state.window_width - 2)) (state.total_width - 1)
                min (state.x_display_anchor + (state.window_width - p_x_dist)) (state.total_width - (state.window_width - p_x_dist))

            else
                state.x_display_anchor

        p_y_dist =
            5

        newYanchor =
            if state.player.location.y <= state.y_display_anchor then
                max 0 (state.y_display_anchor - (state.window_height - p_y_dist))

            else if state.player.location.y >= (state.y_display_anchor + (state.window_height - 1)) then
                --min (state.y_display_anchor + (state.window_height - 2)) (state.total_height - 1)
                min (state.y_display_anchor + (state.window_height - p_y_dist)) (state.total_height - (state.window_height - p_y_dist))

            else
                state.y_display_anchor
    in
    { state | x_display_anchor = newXanchor, y_display_anchor = newYanchor }


cmdGetRandomPositionedPlayer : Player -> Int -> Int -> Int -> Int -> Cmd Msg
cmdGetRandomPositionedPlayer player minX maxX minY maxY =
    if player.placed then
        Cmd.none

    else
        Random.generate NewRandomPointToPlacePlayer (getRandIntPair minX maxX minY maxY)


cmdGetRandomPositionedEnemy : Enemy -> EnemyId -> Int -> Int -> Int -> Int -> Cmd Msg
cmdGetRandomPositionedEnemy actualEnemy enemyId minX maxX minY maxY =
    if actualEnemy.placed then
        Cmd.none

    else
        Random.generate (NewRandomPointToPlaceEnemy enemyId) (getRandIntPair minX maxX minY maxY)


cmdGenerateRandomInitiativeValue : String -> Maybe EnemyId -> Int -> Int -> Cmd Msg
cmdGenerateRandomInitiativeValue strCharacter mbCharacterId minval maxval =
    Random.generate (RandomInitiativeValue strCharacter mbCharacterId) (Random.int minval maxval)


cmdFillRandomIntsPool : GameModel.Model -> Cmd Msg
cmdFillRandomIntsPool state =
    let
        nrToAdd =
            500 - List.length state.pseudoRandomIntsPool
    in
    if nrToAdd > 0 then
        Random.generate NewRandomIntsAddToPool (Random.list nrToAdd (Random.int 1 100))

    else
        Cmd.none


cmdFillRandomIntsPoolAndGenerateRandomMap : GameModel.Model -> Cmd Msg
cmdFillRandomIntsPoolAndGenerateRandomMap state =
    let
        nrToAdd =
            500 - List.length state.pseudoRandomIntsPool
    in
    if nrToAdd > 0 then
        Random.generate NewRandomIntsAddToPoolAndGenerateRandomMap (Random.list nrToAdd (Random.int 1 100))

    else
        Random.generate NewRandomIntsAddToPoolAndGenerateRandomMap (Random.list 1 (Random.int 1 100))


randIntList : Random.Generator (List Int)
randIntList =
    Random.list 10 (Random.int 0 100)


getRandIntPair : Int -> Int -> Int -> Int -> Random.Generator ( Int, Int )
getRandIntPair minX maxX minY maxY =
    Random.pair (Random.int minX maxX) (Random.int minY maxY)


randIntPairsList : Int -> Int -> Random.Generator (List ( Int, Int ))
randIntPairsList nra nrb =
    Random.list 10 (Random.pair (Random.int 0 nra) (Random.int 0 nrb))


cmdGenFloatsForRandomCave : Int -> Int -> Cmd Msg
cmdGenFloatsForRandomCave w h =
    let
        nrFloats =
            w * h
    in
    Random.generate NewRandomFloatsForGenCave (Random.list nrFloats (Random.float 0 1))


move : ( Int, Int ) -> GameModel.Model -> { a | location : GameModel.Location, initiative : Int } -> { a | location : GameModel.Location, initiative : Int }
move ( x, y ) state a =
    let
        location =
            GameModel.location (a.location.x + x) (a.location.y + y)

        initiative =
            a.initiative + 100
    in
    case GameModel.isModelTileWalkable location state of
        False ->
            a

        True ->
            { a
                | location = location
                , initiative = initiative
            }


attack :
    { a | coordination : Int, power : Int, initiative : Int, name : String }
    -> { b | stealth : Int, protection : Int, armor : Int, health : Int, name : String }
    -> List Int
    -> { dudeA : { a | coordination : Int, power : Int, initiative : Int, name : String }, dudeB : { b | stealth : Int, protection : Int, armor : Int, health : Int, name : String }, textMsg : String, randInts : List Int }
attack dude1 dude2 lprandInts =
    let
        ( roll1, newprandInts ) =
            ( List.head lprandInts
                |> Maybe.withDefault 1
            , List.drop 1 lprandInts
            )

        ( roll2, newprandInts2 ) =
            ( List.head newprandInts
                |> Maybe.withDefault 1
            , List.drop 1 newprandInts
            )

        hit =
            if roll1 > dude1.coordination - dude2.stealth then
                False

            else
                True

        guard =
            if dude1.coordination - dude2.stealth > 100 then
                --  dude2.protection - (dude1.coordination - rem dude2.stealth 100)
                dude2.protection - (dude1.coordination - Basics.remainderBy 100 dude2.stealth)

            else
                dude2.protection

        block =
            if hit == True && roll2 < guard then
                True

            else
                False

        dmg =
            if hit && not block then
                dude1.power

            else if hit && block then
                max 0 (dude1.power - dude2.armor)

            else if not hit then
                0

            else
                0

        result =
            dude2.health - dmg

        msg =
            if not hit then
                dude1.name ++ " miss"

            else
                dude1.name ++ " hit " ++ dude2.name ++ " for " ++ String.fromInt dmg ++ " dmg"
    in
    { dudeA = { dude1 | initiative = dude1.initiative + 100 }, dudeB = { dude2 | health = result }, textMsg = msg, randInts = newprandInts2 }


resetEnemyMovesCurrentTurn : GameModel.Model -> GameModel.Model
resetEnemyMovesCurrentTurn state =
    let
        newEnemies =
            Dict.map (\enemyid enemy -> { enemy | nrMovesInCurrentTurn = 0 }) state.enemies
    in
    { state | enemies = newEnemies }


increseNrOfEnemyMovesInCurrentTurn : EnemyId -> GameModel.Model -> GameModel.Model
increseNrOfEnemyMovesInCurrentTurn enemyid state =
    let
        newEnemies =
            Dict.update enemyid (\mbenemy -> mbenemy |> Maybe.map (\en -> { en | nrMovesInCurrentTurn = en.nrMovesInCurrentTurn + 1 })) state.enemies
    in
    { state | enemies = newEnemies }


cleanup : GameModel.Model -> GameModel.Model
cleanup state =
    let
        dead =
            Dict.filter (\enemyId enemy -> enemy.health <= 0) state.enemies

        alive =
            Dict.filter (\enemyId enemy -> enemy.health > 0) state.enemies

        msg =
            if Dict.size dead == 0 then
                Nothing

            else
                Just (Dict.foldl (\id nstr acc -> acc ++ nstr) "" <| Dict.map (\enemyId enemy -> enemy.name ++ " died. ") dead)
    in
    case msg of
        Nothing ->
            state

        Just m ->
            log m { state | enemies = alive }


enemy_AI : GameModel.Model -> GameModel.Model
enemy_AI state =
    let
        mbEnemyIdEnemyPair =
            Dict.filter (\enemyid enemy -> enemy.initiative <= state.player.initiative && enemy.nrMovesInCurrentTurn < enemy.maxNrEnemyMovesPerTurn) state.enemies
                |> Dict.toList
                |> List.head
    in
    case mbEnemyIdEnemyPair of
        Just ( enemyid, enemy ) ->
            let
                state2 =
                    attackIfClose enemy state
                        -- prevent possible infinite recursion
                        |> increseNrOfEnemyMovesInCurrentTurn enemyid
            in
            enemy_AI state2

        Nothing ->
            state


attackIfClose : Enemy -> GameModel.Model -> GameModel.Model
attackIfClose enemy state =
    case List.filter (\location -> location == state.player.location) (Grid.neighborhoodCalc 1 enemy.location) of
        location :: locs ->
            let
                --( enemy_, player_, msg, newprandInts ) =
                attackOutput =
                    attack enemy state.player state.pseudoRandomIntsPool
            in
            log attackOutput.textMsg
                { state
                    | player = attackOutput.dudeB
                    , enemies = Dict.insert enemy.id attackOutput.dudeA state.enemies -- enemy_ :: getTailWithDefaultEmptyList state.enemies
                    , pseudoRandomIntsPool = attackOutput.randInts
                }

        [] ->
            let
                ( x, y, newprandInts ) =
                    ( List.head state.pseudoRandomIntsPool |> Maybe.withDefault 0
                    , List.drop 1 state.pseudoRandomIntsPool
                        |> List.head
                        |> Maybe.withDefault 0
                    , List.drop 2 state.pseudoRandomIntsPool
                    )

                xscaled =
                    if x <= 33 then
                        -- 1/3 probability
                        -1

                    else if x > 33 && x <= 66 then
                        0

                    else
                        1

                yscaled =
                    if y <= 33 then
                        -1

                    else if y > 33 && y <= 66 then
                        0

                    else
                        1

                enemy_ =
                    move ( xscaled, yscaled ) state enemy
            in
            { state
                | enemies = Dict.insert enemy.id enemy_ state.enemies -- enemy_ :: getTailWithDefaultEmptyList state.enemies
                , pseudoRandomIntsPool = newprandInts
            }



-- Right now this just reveals a box around the player


reveal : GameModel.Model -> GameModel.Model
reveal state =
    let
        intermediateModelGrid =
            Grid.map
                (\t ->
                    if GameModel.getTileVisibility t == GameModel.Visible then
                        GameModel.setTileVisibility GameModel.Explored t

                    else
                        t
                )
                state.level

        intermediateModel =
            { state | level = intermediateModelGrid }

        newModel =
            List.foldl (\loc istate -> GameModel.setModelTileVisibility loc GameModel.Visible istate) intermediateModel (GameModel.visible state)
    in
    newModel



{- }
   -- Right now this just reveals a box around the player


   reveal : GameModel.Model -> GameModel.Model
   reveal state =
       let
           exploredAcc =
               Grid.map
                   (\t ->
                       if t == GameModel.Visible then
                           GameModel.Explored
                       else
                           t
                   )
                   state.explored

           explored_ =
               List.foldl (\l explored -> Grid.set l GameModel.Visible explored) exploredAcc (GameModel.visible state)
       in
       { state | explored = explored_ }
-}
