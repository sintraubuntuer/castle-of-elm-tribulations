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
--import Collage.Layout

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Collage
import Dict exposing (Dict)
import GameDefinitions.Game1.Game1Definitions
import GameDefinitions.Game2.Game2Definitions
import GameModel
    exposing
        ( Model
        , defaultGrassInfo
        )
import Grid
import Item exposing (Item(..), KeyInfo)
import MapGen
import Random
import Thorns.ThornGrid as ThornGrid
import Thorns.Types
import Thorns.Update as ThornsUpdate
import Tile exposing (Tile(..), Visibility(..))


log : String -> Model -> Model
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
    | CleanUpAndEnemyLogic
    | StartOpponentInteraction Enemy
    | ChangeFloorTo FloorId ( Int, Int )
    | NewRandomPointToPlacePlayer ( Int, Int )
    | NewRandomPointToPlaceEnemy EnemyId ( Int, Int )
    | NewRandomFloatsForGenCave (List Float)
    | RandomInitiativeValue String (Maybe EnemyId) Int
    | NewRandomIntsAddToPool (List Int)
    | NewRandomIntsAddToPoolAndGenerateRandomMap (List Int)
    | ThornsMsg Thorns.Types.Msg


modelChangerFuncsByGameIdAndLeverId : Int -> Int -> List (Grid.Coordinate -> GameModel.Model -> GameModel.Model)
modelChangerFuncsByGameIdAndLeverId gId lId =
    if gId == 2 && lId == 1 then
        modelChangerFuncsForGame1Level1

    else
        []


customLeverInfo : Tile.LeverInfo
customLeverInfo =
    { isUp = False
    , isTransparent = False
    , isExplored = False
    , visibility = Visible
    }


modelChangerFuncsForGame1Level1 : List (Grid.Coordinate -> GameModel.Model -> GameModel.Model)
modelChangerFuncsForGame1Level1 =
    let
        nrEnlightenedOpponents model =
            Dict.values model.enemies |> List.filter (\en -> en.indexOfLight >= en.indexOfLightMax) |> List.length

        nrDeadOpponents model =
            Dict.values model.enemies |> List.filter (\en -> en.health <= 0) |> List.length

        reqsCompleted model =
            nrEnlightenedOpponents model >= 3 && nrDeadOpponents model == 0
    in
    [ \coords model ->
        if reqsCompleted model then
            { model
                | level =
                    Grid.set (Grid.Coordinate 15 2) (Tile.Floor GameModel.defaultFloorInfo) model.level
                        |> Grid.set (Grid.Coordinate 16 2) (Tile.Water GameModel.walkableWaterInfo)
                        |> Grid.set (Grid.Coordinate 17 2) (Tile.Water GameModel.walkableWaterInfo)
                        |> Grid.set (Grid.Coordinate 20 13) (Tile.Grass defaultGrassInfo)
            }

        else
            { model
                | level =
                    Grid.set (Grid.Coordinate 15 2) (Tile.Floor GameModel.defaultFloorInfo) model.level
                        |> Grid.set coords (Lever customLeverInfo)
            }
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
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

                            newDisplay =
                                --model.gameOfThornsModeisOn && not newThornsModel.interactionHasFinished
                                if model.currentDisplay == GameModel.DisplayGameOfThorns && not newThornsModel.interactionHasFinished then
                                    GameModel.DisplayGameOfThorns

                                else if model.currentDisplay == GameModel.DisplayGameOfThorns && newThornsModel.interactionHasFinished then
                                    if newPlayer.health <= 0 then
                                        GameModel.DisplayGameOver

                                    else
                                        GameModel.DisplayRegularGame

                                else
                                    model.currentDisplay
                        in
                        { model
                            | enemies = newEnemies
                            , otherCharacters = newOtherCharacters
                            , player = newPlayer
                            , gameOfThornsModel = newThornsModel
                            , currentDisplay = newDisplay
                            , listeningToKeyInput = True
                        }
                            |> cleanup

                    else
                        { model | gameOfThornsModel = newThornsModel }
            in
            ( newModel, Cmd.map ThornsMsg thorns_cmds )

        StartGameNr nr ->
            let
                ( initModel, createRandomMap, randomlyPositionPlayer ) =
                    case nr of
                        1 ->
                            GameDefinitions.Game1.Game1Definitions.initialModelFunc

                        2 ->
                            GameDefinitions.Game2.Game2Definitions.initialModelFunc model.pseudoRandomIntsPool

                        _ ->
                            ( model, False, False )

                gBounds =
                    Grid.getGridBoundsToPlacePlayer initModel.level
            in
            ( initModel |> position_display_anchor_in_order_to_center_player
            , Cmd.batch
                ([ if createRandomMap then
                    cmdFillRandomIntsPoolAndGenerateRandomMap initModel

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

                    GameModel.ViewStatsOverlay ->
                        ( { model | displayStatsOverlay = not model.displayStatsOverlay }, Cmd.none )

                    GameModel.ViewHelpMode ->
                        ( { model
                            | currentDisplay =
                                if model.currentDisplay == GameModel.DisplayHelpScreen then
                                    GameModel.DisplayRegularGame

                                else
                                    GameModel.DisplayHelpScreen
                          }
                        , Cmd.none
                        )

                    GameModel.ViewOpponentReport ->
                        ( { model
                            | currentDisplay =
                                if model.currentDisplay == GameModel.DisplayOpponentReport then
                                    GameModel.DisplayRegularGame

                                else
                                    GameModel.DisplayOpponentReport
                          }
                        , Cmd.none
                        )

                    GameModel.ViewInventory ->
                        --( { model | displayInventory = not model.displayInventory }, Cmd.none )
                        ( { model
                            | currentDisplay =
                                if model.currentDisplay == GameModel.DisplayInventory then
                                    GameModel.DisplayRegularGame

                                else
                                    GameModel.DisplayInventory
                          }
                        , Cmd.none
                        )

                    GameModel.Nop ->
                        ( model, Cmd.none )

        TryAddToPlayerInventory ->
            let
                player_ =
                    model.player

                pcoords =
                    model.player.location

                --checkIfTheresAnItemLocatedAt pcoords
                ( updatedInventory, newGrid, newHealth ) =
                    case Grid.get pcoords model.level of
                        Just (Tile.Floor floorinfo) ->
                            case floorinfo.item of
                                Just item ->
                                    case item of
                                        Paper paperinfo ->
                                            ( Dict.update ("paper_" ++ String.fromInt paperinfo.id) (\_ -> Just item) model.player.inventory
                                            , Grid.set pcoords (Tile.Floor { floorinfo | item = Nothing }) model.level
                                            , model.player.health
                                            )

                                        Food fdescription ->
                                            -- consume imediatly the item which adds to the player health
                                            ( model.player.inventory
                                            , Grid.set pcoords (Tile.Floor { floorinfo | item = Nothing }) model.level
                                            , model.player.health + 4
                                            )

                                        _ ->
                                            ( Dict.update (GameModel.itemToString item) (\_ -> Just item) model.player.inventory
                                            , Grid.set pcoords (Tile.Floor { floorinfo | item = Nothing }) model.level
                                            , model.player.health
                                            )

                                _ ->
                                    ( model.player.inventory, model.level, model.player.health )

                        _ ->
                            ( model.player.inventory, model.level, model.player.health )

                newPlayer =
                    { player_ | inventory = updatedInventory, health = newHealth }
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
                        Just (Tile.Lever leverinfo) ->
                            if leverinfo.isUp then
                                model
                                --|> Debug.log " you just interacted with an up lever "

                            else
                                { model | level = Grid.set (GameModel.location x2 y2) (Tile.Lever { leverinfo | isUp = True }) model.level }
                                    |> (\xmodel -> List.foldl (\cfunc modacc -> cfunc (GameModel.location x2 y2) modacc) xmodel (modelChangerFuncsByGameIdAndLeverId 2 1))

                        --|> turnNeighbourWallCellstoAshes (GameModel.location x2 y2)
                        --  |> Debug.log " you just interacted with a down lever "
                        _ ->
                            model
                                |> (\model_ ->
                                        case Grid.get (GameModel.location x2 y2) model_.level of
                                            Just (Tile.ConverterTile initialTile newTile) ->
                                                { model_ | level = Grid.set (GameModel.location x2 y2) newTile model_.level }

                                            _ ->
                                                model_
                                   )

                mbEnemy =
                    case Dict.filter (\enemyid enemy -> enemy.floorId == model.currentFloorId && enemy.location == GameModel.location x2 y2) model.enemies |> Dict.values of
                        [] ->
                            Nothing

                        enemy :: es ->
                            Just enemy

                ( newModel2, themsg ) =
                    case mbEnemy of
                        Just enemy ->
                            if enemy.health > 0 && enemy.indexOfLight < enemy.indexOfLightMax && newModel.player.health > 0 then
                                ( newModel, StartOpponentInteraction enemy )

                            else if enemy.health <= 0 && enemy.playerCanWalkOverIfDead && (x_ /= 0 || y_ /= 0) then
                                ( { newModel | player = move ( x_, y_ ) newModel newModel.player }
                                    |> checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor
                                    |> openDoorIfPlayerStandingOnDoorAndClosed
                                    --|> checkIfPlayerStandsOnStairsAndMoveToNewFloor
                                    |> checkAndAlterDisplayAnchorIfNecessary
                                , CleanUpAndEnemyLogic
                                )

                            else
                                ( newModel, CleanUpAndEnemyLogic )

                        Nothing ->
                            if x_ /= 0 || y_ /= 0 then
                                ( { newModel | player = move ( x_, y_ ) newModel newModel.player }
                                    |> checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor
                                    |> openDoorIfPlayerStandingOnDoorAndClosed
                                    --|> checkIfPlayerStandsOnStairsAndMoveToNewFloor
                                    |> checkAndAlterDisplayAnchorIfNecessary
                                , CleanUpAndEnemyLogic
                                )

                            else
                                ( newModel, CleanUpAndEnemyLogic )
            in
            update themsg newModel2

        CleanUpAndEnemyLogic ->
            let
                ( newModel, lenemies ) =
                    model
                        |> cleanup
                        |> resetEnemyMovesCurrentTurn
                        |> enemy_AI
                        |> (\( modl, le ) ->
                                if modl.currentDisplay == GameModel.DisplayGameOver then
                                    ( { modl | listeningToKeyInput = False }, le )

                                else
                                    ( modl, le )
                           )

                isGameCompleted =
                    checkGameCompletion model
            in
            if isGameCompleted then
                ( { newModel
                    | currentDisplay = GameModel.DisplayGameCompleted
                    , enemies = Dict.empty
                    , otherCharacters = Dict.empty
                  }
                , Cmd.none
                )

            else
                case lenemies |> List.head of
                    Just enemy_ ->
                        update (StartOpponentInteraction enemy_) newModel

                    Nothing ->
                        ( newModel |> reveal, cmdFillRandomIntsPool newModel )

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
                        | currentDisplay = GameModel.DisplayGameOfThorns --gameOfThornsModeisOn = True
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
            case GameModel.isModelTileWalkable newLocation newPlayer model of
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
                    case GameModel.isModelTileWalkable newLocation actualEnemy model of
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


checkGameCompletion : Model -> Bool
checkGameCompletion model =
    model.gameCompletionFunc model.currentFloorId model.player.location


isPlayerStandingOnStairs : Model -> Bool
isPlayerStandingOnStairs model =
    let
        mbTile =
            Grid.get model.player.location model.level
    in
    case mbTile of
        Just (Tile.Stairs sinfo) ->
            True

        _ ->
            False


checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor : Model -> Model
checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor model =
    let
        mbTile =
            Grid.get model.player.location model.level
    in
    case mbTile of
        Just (Tile.Stairs sinfo) ->
            let
                mbDestinationCoordsTuple =
                    searchFloorForStairsId sinfo.toFloorId sinfo.toStairsId model
            in
            case mbDestinationCoordsTuple of
                Just ( newX, newY ) ->
                    changeFloorTo model sinfo.toFloorId ( newX + Tuple.first sinfo.shift, newY + Tuple.second sinfo.shift )

                Nothing ->
                    model

        Just (Tile.Hole hinfo) ->
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
                    changeFloorTo model floorId ( newX, newY )

                Nothing ->
                    model

        Just (Tile.Wall wallinfo) ->
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
                            changeFloorTo model floorId ( newX, newY )

                        Nothing ->
                            model

        _ ->
            model


searchFloorForTeleporterId : Int -> Int -> Model -> Maybe ( Int, Int, Int )
searchFloorForTeleporterId fid target_id_ model =
    let
        mbFloorGrid =
            Dict.get fid model.floorDict

        getlcoords fgrid =
            Grid.toCoordinates fgrid

        checkCoords coords_ fgrid =
            case Grid.get coords_ fgrid of
                Just (Tile.Wall wallinfo) ->
                    case wallinfo.mbTeleporterObject of
                        Just ateleporter ->
                            ateleporter.teleporter_id == target_id_

                        Nothing ->
                            False

                _ ->
                    False
    in
    case mbFloorGrid of
        Just floorGrid ->
            List.filter (\coords -> checkCoords coords floorGrid.level) (getlcoords floorGrid.level)
                |> List.head
                |> Maybe.map (\rec -> ( fid, rec.x, rec.y ))

        Nothing ->
            Nothing


searchFloorForTargetId : Int -> Int -> Model -> Maybe ( Int, Int, Int )
searchFloorForTargetId fid target_id model =
    let
        mbFloorGrid =
            Dict.get fid model.floorDict

        getlcoords fgrid =
            Grid.toCoordinates fgrid

        checkCoords coords_ fgrid =
            case Grid.get coords_ fgrid of
                Just (Tile.Floor finfo) ->
                    case finfo.floorDrawing of
                        Just (Tile.LandingTargetDrawing tid) ->
                            target_id == tid

                        _ ->
                            False

                _ ->
                    False
    in
    case mbFloorGrid of
        Just floorGrid ->
            List.filter (\coords -> checkCoords coords floorGrid.level) (getlcoords floorGrid.level)
                |> List.head
                |> Maybe.map (\rec -> ( fid, rec.x, rec.y ))

        Nothing ->
            Nothing


searchFloorForStairsId : Int -> Int -> Model -> Maybe ( Int, Int )
searchFloorForStairsId floorId stairsId model =
    let
        mbFloorGrid =
            Dict.get floorId model.floorDict

        getlcoords fgrid =
            Grid.toCoordinates fgrid

        checkCoords coords_ fgrid =
            case Grid.get coords_ fgrid of
                Just (Tile.Stairs sinfo) ->
                    sinfo.stairsId == stairsId

                _ ->
                    False
    in
    case mbFloorGrid of
        Just floorGrid ->
            List.filter (\coords -> checkCoords coords floorGrid.level) (getlcoords floorGrid.level)
                |> List.head
                |> Maybe.map (\rec -> ( rec.x, rec.y ))

        Nothing ->
            Nothing


changeFloorTo : Model -> Int -> ( Int, Int ) -> Model
changeFloorTo model floorId locTuple =
    let
        newModel =
            if model.currentFloorId == floorId then
                model

            else
                let
                    currentFloorInfo =
                        GameModel.getCurrentFloorInfoToStore model

                    newStore =
                        Dict.update model.currentFloorId (\_ -> Just currentFloorInfo) model.floorDict

                    newCurrentFloor =
                        Dict.get floorId model.floorDict
                in
                case newCurrentFloor of
                    Just cFloor ->
                        { model
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
                        { model | floorDict = newStore }

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


position_display_anchor_in_order_to_center_player : Model -> Model
position_display_anchor_in_order_to_center_player model =
    { model
        | x_display_anchor = max 0 (model.player.location.x - round (toFloat model.window_width / 2.0))
        , y_display_anchor = max 0 (model.player.location.y - round (toFloat model.window_height / 2))
    }
        |> reveal


turnNeighbourWallCellstoAshes : Grid.Coordinate -> Model -> Model
turnNeighbourWallCellstoAshes { x, y } model =
    let
        upCell =
            GameModel.location x (y - 1)

        downCell =
            GameModel.location x (y + 1)

        leftCell =
            GameModel.location (x - 1) y

        rightCell =
            GameModel.location (x + 1) y

        convertCellsFunc cellCoords themodel =
            case Grid.get cellCoords themodel.level of
                Just (Tile.Wall wallinfo) ->
                    let
                        floorinfo =
                            GameModel.defaultFloorInfo
                    in
                    { themodel | level = Grid.set cellCoords (Tile.Floor { floorinfo | item = Just Ash }) themodel.level }
                        |> turnNeighbourWallCellstoAshes cellCoords

                _ ->
                    themodel
    in
    convertCellsFunc upCell model
        |> convertCellsFunc downCell
        |> convertCellsFunc leftCell
        |> convertCellsFunc rightCell
        |> updateWallPercentageValue


updateWallPercentageValue : Model -> Model
updateWallPercentageValue model =
    let
        thegrid =
            model.level

        gridAsList =
            Grid.toList thegrid |> List.concatMap identity

        wallPercentage =
            getWallPercentage gridAsList
    in
    { model | wallPercentage = Just wallPercentage }


getWallPercentage : List Tile -> Float
getWallPercentage gridAsList =
    let
        getCountTuple : Tile -> ( Int, Int ) -> ( Int, Int )
        getCountTuple elem ( acc1, acc2 ) =
            case elem of
                Tile.Wall _ ->
                    ( acc1 + 1, acc2 )

                Tile.Floor _ ->
                    ( acc1, acc2 + 1 )

                _ ->
                    ( acc1, acc2 )
    in
    gridAsList
        |> List.foldl (\el ( ac1, ac2 ) -> getCountTuple el ( ac1, ac2 )) ( 0, 0 )
        |> (\tup -> (Tuple.first tup |> toFloat) / (Tuple.second tup |> toFloat))


checkAndAlterDisplayAnchorIfNecessary : Model -> Model
checkAndAlterDisplayAnchorIfNecessary model =
    let
        p_x_dist =
            5

        newXanchor =
            if model.player.location.x <= model.x_display_anchor then
                max 0 (model.x_display_anchor - (model.window_width - p_x_dist))

            else if model.player.location.x >= (model.x_display_anchor + (model.window_width - 1)) then
                --min (model.x_display_anchor + (model.window_width - 2)) (model.total_width - 1)
                min (model.x_display_anchor + (model.window_width - p_x_dist)) (model.total_width - (model.window_width - p_x_dist))

            else
                model.x_display_anchor

        p_y_dist =
            5

        newYanchor =
            if model.player.location.y <= model.y_display_anchor then
                max 0 (model.y_display_anchor - (model.window_height - p_y_dist))

            else if model.player.location.y >= (model.y_display_anchor + (model.window_height - 1)) then
                --min (model.y_display_anchor + (model.window_height - 2)) (model.total_height - 1)
                min (model.y_display_anchor + (model.window_height - p_y_dist)) (model.total_height - (model.window_height - p_y_dist))

            else
                model.y_display_anchor
    in
    { model | x_display_anchor = newXanchor, y_display_anchor = newYanchor }


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


cmdFillRandomIntsPool : Model -> Cmd Msg
cmdFillRandomIntsPool model =
    let
        nrToAdd =
            500 - List.length model.pseudoRandomIntsPool
    in
    if nrToAdd > 0 then
        Random.generate NewRandomIntsAddToPool (Random.list nrToAdd (Random.int 1 100))

    else
        Cmd.none


cmdFillRandomIntsPoolAndGenerateRandomMap : Model -> Cmd Msg
cmdFillRandomIntsPoolAndGenerateRandomMap model =
    let
        nrToAdd =
            500 - List.length model.pseudoRandomIntsPool
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


move : ( Int, Int ) -> Model -> { a | location : GameModel.Location, direction : Beings.Direction, inventory : Beings.Inventory, initiative : Int } -> { a | location : GameModel.Location, direction : Beings.Direction, inventory : Beings.Inventory, initiative : Int }
move ( x_shift, y_shift ) model a =
    let
        location =
            GameModel.location (a.location.x + x_shift) (a.location.y + y_shift)

        initiative =
            a.initiative + 100
    in
    case GameModel.isModelTileWalkable location a model of
        False ->
            a

        True ->
            { a
                | location = location
                , initiative = initiative
                , direction =
                    if x_shift > 0 then
                        Beings.Right

                    else if x_shift < 0 then
                        Beings.Left

                    else if y_shift > 0 then
                        Beings.Down

                    else
                        Beings.Up
            }


openDoorIfPlayerStandingOnDoorAndClosed : Model -> Model
openDoorIfPlayerStandingOnDoorAndClosed model =
    let
        newGrid =
            case Grid.get model.player.location model.level of
                Just (Tile.Door doorinfo) ->
                    if not doorinfo.isOpen then
                        let
                            newDoorInfo =
                                { doorinfo | isOpen = True }
                        in
                        Grid.set model.player.location (Tile.Door newDoorInfo) model.level

                    else
                        model.level

                _ ->
                    model.level
    in
    { model | level = newGrid }


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


resetEnemyMovesCurrentTurn : Model -> Model
resetEnemyMovesCurrentTurn model =
    let
        newEnemies =
            Dict.map (\enemyid enemy -> { enemy | nrMovesInCurrentTurn = 0 }) model.enemies
    in
    { model | enemies = newEnemies }


increseNrOfEnemyMovesInCurrentTurn : EnemyId -> Model -> Model
increseNrOfEnemyMovesInCurrentTurn enemyid model =
    let
        newEnemies =
            Dict.update enemyid (\mbenemy -> mbenemy |> Maybe.map (\en -> { en | nrMovesInCurrentTurn = en.nrMovesInCurrentTurn + 1 })) model.enemies
    in
    { model | enemies = newEnemies }


cleanup : Model -> Model
cleanup model =
    let
        dead_and_disappears =
            Dict.filter (\enemyId enemy -> enemy.health <= 0 && enemy.disappearsWhenHealthIsZero) model.enemies

        dead_and_doesnt_disappear =
            Dict.filter (\enemyId enemy -> enemy.health <= 0 && not enemy.disappearsWhenHealthIsZero) model.enemies

        alive_no_enlightenment =
            Dict.filter (\enemyId enemy -> enemy.health > 0 && enemy.indexOfLight < enemy.indexOfLightMax) model.enemies

        alive_enlightened_disappears =
            Dict.filter (\enemyId enemy -> enemy.health > 0 && enemy.indexOfLight >= enemy.indexOfLightMax && enemy.disappearsWhenIndexOfLightMax) model.enemies

        alive_enlightened_doesnt_disappear =
            Dict.filter (\enemyId enemy -> enemy.health > 0 && enemy.indexOfLight >= enemy.indexOfLightMax && not enemy.disappearsWhenIndexOfLightMax) model.enemies

        keys_to_remove =
            (dead_and_disappears |> Dict.keys) ++ (alive_enlightened_disappears |> Dict.keys)

        msg =
            if Dict.size dead_and_disappears == 0 then
                Nothing

            else
                Just (Dict.foldl (\id nstr acc -> acc ++ nstr) "" <| Dict.map (\enemyId enemy -> enemy.name ++ " died. ") dead_and_disappears)

        --newModel =  { model | enemies = ( alive ++ dead_and_doesnt_disappear ++ alive_no_enlightenment ++  alive_enlightened_doesnt_disappear )  }
        newModel =
            { model | enemies = Dict.filter (\k v -> not (inList k keys_to_remove)) model.enemies }
    in
    case msg of
        Nothing ->
            newModel

        Just m ->
            log m newModel


enemyExceedsNrMovesInCurrentTurn : EnemyId -> Model -> Bool
enemyExceedsNrMovesInCurrentTurn enemyid model =
    let
        mbEnemy =
            Dict.get enemyid model.enemies
    in
    case mbEnemy of
        Nothing ->
            True

        Just enemy ->
            enemy.nrMovesInCurrentTurn >= enemy.maxNrEnemyMovesPerTurn


enemy_AI : Model -> ( Model, List Enemy )
enemy_AI model =
    let
        enemyIdEnemyPairList =
            Dict.filter (\enemyid enemy -> enemy.health > 0 && enemy.nrMovesInCurrentTurn < enemy.maxNrEnemyMovesPerTurn) model.enemies
                |> Dict.toList

        --&& enemy.initiative <= model.player.initiative
        --|> List.head
        ai_helper_func : EnemyId -> ( Model, List Enemy ) -> ( Model, List Enemy )
        ai_helper_func enemyid ( model_, lmbe ) =
            if enemyExceedsNrMovesInCurrentTurn enemyid model_ || model.currentDisplay == GameModel.DisplayGameOfThorns then
                -- model_.gameOfThornsModeisOn then
                ( model_, lmbe )

            else
                let
                    mbenemy =
                        Dict.get enemyid model_.enemies

                    ( model2, mbenemyForGameOfThorns ) =
                        case mbenemy of
                            Nothing ->
                                ( model_, Nothing )

                            Just enemy ->
                                if enemy.floorId == model_.currentFloorId && enemy.indexOfLight < enemy.indexOfLightMax && model.player.health > 0 && model.currentDisplay /= GameModel.DisplayGameOfThorns then
                                    attackIfClose enemy model_
                                        -- prevent possible infinite recursion
                                        |> (\( x, y ) -> ( increseNrOfEnemyMovesInCurrentTurn enemyid x, y ))

                                else
                                    ( enemyMove enemy model_
                                        |> increseNrOfEnemyMovesInCurrentTurn enemyid
                                    , Nothing
                                    )
                in
                case mbenemyForGameOfThorns of
                    Just en ->
                        ( model2, lmbe ++ [ en ] )

                    Nothing ->
                        --ai_helper_func enemyid ( model2, lmbe )
                        ai_helper_func enemyid ( model2, lmbe )
    in
    List.foldl (\enpair ( modelacc, lmbenemies ) -> ai_helper_func (Tuple.first enpair) ( modelacc, lmbenemies )) ( model, [] ) enemyIdEnemyPairList


attackIfClose : Enemy -> Model -> ( Model, Maybe Enemy )
attackIfClose enemy model =
    if enemy.floorId /= model.currentFloorId && enemy.health > 0 then
        ( enemyMove enemy model, Nothing )

    else
        case List.filter (\location -> location == model.player.location) (Grid.neighborhoodCalc 1 enemy.location) of
            location :: locs ->
                if enemy.attacksUsingGameOfThorns then
                    ( model, Just enemy )

                else
                    let
                        --( enemy_, player_, msg, newprandInts ) =
                        attackOutput =
                            attack enemy model.player model.pseudoRandomIntsPool
                    in
                    ( log attackOutput.textMsg
                        { model
                            | player = attackOutput.dudeB
                            , enemies = Dict.insert enemy.id attackOutput.dudeA model.enemies -- enemy_ :: getTailWithDefaultEmptyList model.enemies
                            , pseudoRandomIntsPool = attackOutput.randInts
                        }
                    , Nothing
                    )

            [] ->
                ( enemyMove enemy model, Nothing )


enemyMove : Enemy -> Model -> Model
enemyMove enemy model =
    let
        ( xrand, yrand, newprandInts ) =
            ( List.head model.pseudoRandomIntsPool |> Maybe.withDefault 0
            , List.drop 1 model.pseudoRandomIntsPool
                |> List.head
                |> Maybe.withDefault 0
            , List.drop 2 model.pseudoRandomIntsPool
            )

        x_delta_toPlayer =
            model.player.location.x - enemy.location.x

        y_delta_toPlayer =
            model.player.location.y - enemy.location.y

        xscaled =
            if x_delta_toPlayer > 0 then
                if xrand <= 85 then
                    -- 1/3 probability
                    1

                else
                    -1

            else if x_delta_toPlayer < 0 then
                if xrand <= 85 then
                    -- 1/3 probability
                    -1

                else
                    1

            else if xrand <= 33 then
                -1

            else if xrand > 33 && xrand <= 66 then
                0

            else
                1

        yscaled =
            if y_delta_toPlayer > 0 then
                if yrand <= 85 then
                    1

                else
                    -1

            else if y_delta_toPlayer < 0 then
                if yrand <= 85 then
                    -1

                else
                    1

            else if yrand <= 33 then
                -1

            else if yrand > 33 && yrand <= 66 then
                0

            else
                1

        enemy_ =
            move ( xscaled, yscaled ) model enemy
                |> (\en ->
                        if en.location == model.player.location then
                            enemy

                        else
                            en
                   )
    in
    { model
        | enemies = Dict.insert enemy.id enemy_ model.enemies -- enemy_ :: getTailWithDefaultEmptyList model.enemies
        , pseudoRandomIntsPool = newprandInts
    }



-- Right now this just reveals a box around the player


reveal : Model -> Model
reveal model =
    let
        intermediateModelGrid =
            Grid.map
                (\t ->
                    if GameModel.getTileVisibility t == Visible then
                        GameModel.setTileVisibility Explored t

                    else
                        t
                )
                model.level

        intermediateModel =
            { model | level = intermediateModelGrid }

        newModel =
            List.foldl (\loc imodel -> GameModel.setModelTileVisibility loc Visible imodel) intermediateModel (GameModel.visible model)
    in
    newModel


inList : a -> List a -> Bool
inList a_val la =
    List.filter (\elem -> elem == a_val) la
        |> List.length
        |> (\x -> x > 0)



{- }
   -- Right now this just reveals a box around the player


   reveal : Model -> Model
   reveal model =
       let
           exploredAcc =
               Grid.map
                   (\t ->
                       if t == Visible then
                           Explored
                       else
                           t
                   )
                   model.explored

           explored_ =
               List.foldl (\l explored -> Grid.set l Visible explored) exploredAcc (GameModel.visible model)
       in
       { model | explored = explored_ }
-}
