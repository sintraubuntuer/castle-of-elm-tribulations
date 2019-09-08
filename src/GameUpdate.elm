module GameUpdate exposing
    ( Msg(..)
    , checkAndAlterViewportAnchorIfNecessary
    , cleanup
    , cmdFillRandomIntsPool
    , cmdFillRandomIntsPoolAndGenerateRandomMap
    , cmdGenFloatsForRandomCave
    , cmdGenerateRandomInitiativeValue
    , cmdGetRandomPositionedFightingCharacter
    , cmdGetRandomPositionedPlayer
    , fightingCharacter_AI
    , getRandIntPair
    , getTailWithDefaultEmptyList
    , increseNrOfFightingCharacterMovesInCurrentTurn
    , log
    , move
    , randIntList
    , randIntPairsList
    , resetFightingCharacterMovesCurrentTurn
    , reveal
    , update
    )

import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Beings.BeingsInTileGrid as BeingsInTileGrid
import Beings.FightingCharacterInTileGrid as FightingCharacterInTileGrid
import Beings.OtherCharacterInTileGrid as OtherCharacterInTileGrid
import Delay
import Dict exposing (Dict)
import GameDefinitions.Game1.Game1Definitions
import GameDefinitions.Game2.Game2Definitions
import GameModel exposing (Model)
import Grid
import Item exposing (Item(..), KeyInfo)
import MapGen
import Random
import Thorns.ThornGrid as ThornGrid
import Thorns.Types
import Thorns.Update as ThornsUpdate
import Tile exposing (Tile(..), Visibility(..))
import Time


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
    | CleanUpAndFightingCharacterLogic
    | StartOpponentInteraction FightingCharacter
    | ChangeFloorTo FloorId ( Int, Int )
    | ShowMap
    | AboutToStartGameNr Int String String
    | NewRandomPointToPlacePlayer ( Int, Int )
    | NewGridCoordinatesIndexToPlaceFightingCharacter FightingCharacterId Int
    | NewRandomPointToPlaceFightingCharacter FightingCharacterId ( Int, Int )
    | NewRandomFloatsForGenCave (List Float)
    | RandomInitiativeValue String (Maybe FightingCharacterId) Int
    | NewRandomIntsAddToPool (List Int)
    | NewRandomIntsAddToPoolAndGenerateRandomMap (List Int)
    | ThornsMsg Thorns.Types.Msg


getModelChangerFuncs : Tile.LeverId -> Model -> List (Grid.Coordinate -> Model -> Model)
getModelChangerFuncs leverId model =
    Dict.get leverId model.leverModelChangerFuncs
        |> (\x ->
                case x of
                    Just (GameModel.SimpleModelChanger lfuncs) ->
                        lfuncs

                    Nothing ->
                        []
           )


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
                            ( updatedFightCharacters, newOtherCharacters, newPlayer ) =
                                case newThornsModel.opponent of
                                    Just (Thorns.Types.FightingCharacter erec) ->
                                        ( Dict.update erec.id (\_ -> Just erec) model.fightingCharacters
                                        , model.otherCharacters
                                        , newThornsModel.player
                                        )

                                    Just (Thorns.Types.Ochar orec) ->
                                        ( model.fightingCharacters
                                        , Dict.update orec.id (\_ -> Just orec) model.otherCharacters
                                        , newThornsModel.player
                                        )

                                    Nothing ->
                                        ( model.fightingCharacters, model.otherCharacters, model.player )

                            newDisplay =
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
                            | fightingCharacters = updatedFightCharacters
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

        AboutToStartGameNr nr gname imgStr ->
            ( { model
                | currentDisplay = GameModel.DisplayAboutToStartGame nr gname imgStr
                , started = True
              }
            , Cmd.batch
                [ Delay.after 200.0 Delay.Millisecond (StartGameNr nr) ]
            )

        StartGameNr nr ->
            let
                ( initModel, createRandomMap, randomlyPositionPlayer ) =
                    case nr of
                        1 ->
                            GameDefinitions.Game1.Game1Definitions.initialModelFunc model.imgBaseDir

                        2 ->
                            GameDefinitions.Game2.Game2Definitions.initialModelFunc model.pseudoRandomIntsPool model.imgBaseDir

                        _ ->
                            ( model, False, False )

                gBounds =
                    Grid.getGridBoundsToPlacePlayer initModel.level

                floorGridCoordinates floorId =
                    if floorId == model.currentFloorId then
                        Grid.toCoordinates model.level

                    else
                        Dict.get floorId model.floorDict
                            |> Maybe.map (\g -> Grid.toCoordinates g.level)
                            |> Maybe.withDefault []
            in
            ( initModel |> position_viewport_in_order_to_center_player
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
                    ++ (Dict.map (\fcharId fightingCharacter -> cmdGetRandomPositionedFightingCharacter fightingCharacter fcharId (floorGridCoordinates fightingCharacter.floorId |> List.length)) initModel.fightingCharacters
                            |> Dict.values
                       )
                    ++ (Dict.map (\fcharId fightingCharacter -> cmdGenerateRandomInitiativeValue "fightingCharacter" (Just fcharId) 1 100) initModel.fightingCharacters
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
                        if model.currentDisplay == GameModel.DisplayRegularGame || model.currentDisplay == GameModel.DisplayGameCompleted then
                            update (TryShiftPlayerPosition ( 0, 0 - 1 )) model

                        else
                            ( model, Cmd.none )

                    GameModel.Down ->
                        if model.currentDisplay == GameModel.DisplayRegularGame || model.currentDisplay == GameModel.DisplayGameCompleted then
                            update (TryShiftPlayerPosition ( 0, 0 + 1 )) model

                        else
                            ( model, Cmd.none )

                    GameModel.Left ->
                        if model.currentDisplay == GameModel.DisplayRegularGame || model.currentDisplay == GameModel.DisplayGameCompleted then
                            update (TryShiftPlayerPosition ( 0 - 1, 0 )) model

                        else
                            ( model, Cmd.none )

                    GameModel.Right ->
                        if model.currentDisplay == GameModel.DisplayRegularGame || model.currentDisplay == GameModel.DisplayGameCompleted then
                            update (TryShiftPlayerPosition ( 0 + 1, 0 )) model

                        else
                            ( model, Cmd.none )

                    GameModel.PickUpItem ->
                        if model.currentDisplay == GameModel.DisplayRegularGame then
                            update TryAddToPlayerInventory model

                        else
                            ( model, Cmd.none )

                    GameModel.ViewStatsOverlay ->
                        ( { model | displayStatsOverlay = not model.displayStatsOverlay }, Cmd.none )

                    GameModel.ViewHelpMode ->
                        ( { model
                            | currentDisplay =
                                if model.currentDisplay == GameModel.DisplayHelpScreen then
                                    GameModel.DisplayRegularGame

                                else if model.currentDisplay == GameModel.DisplayRegularGame then
                                    GameModel.DisplayHelpScreen

                                else
                                    model.currentDisplay
                          }
                        , Cmd.none
                        )

                    GameModel.ViewOpponentReport ->
                        ( { model
                            | currentDisplay =
                                if model.currentDisplay == GameModel.DisplayOpponentReport then
                                    GameModel.DisplayRegularGame

                                else if model.currentDisplay == GameModel.DisplayRegularGame then
                                    GameModel.DisplayOpponentReport

                                else
                                    model.currentDisplay
                          }
                        , Cmd.none
                        )

                    GameModel.ViewInventory ->
                        ( { model
                            | currentDisplay =
                                if model.currentDisplay == GameModel.DisplayInventory then
                                    GameModel.DisplayRegularGame

                                else if model.currentDisplay == GameModel.DisplayRegularGame then
                                    GameModel.DisplayInventory

                                else
                                    model.currentDisplay
                          }
                        , Cmd.none
                        )

                    GameModel.ViewMap ->
                        let
                            newModel =
                                if model.currentDisplay == GameModel.DisplayMap then
                                    { model
                                        | currentDisplay = GameModel.DisplayRegularGame
                                        , tileWidth = model.tileWidth * 8
                                        , tileHeight = model.tileHeight * 8
                                        , viewport_width = model.viewport_width // 8
                                        , viewport_height = model.viewport_height // 8
                                    }

                                else if model.currentDisplay == GameModel.DisplayRegularGame then
                                    { model
                                        | currentDisplay = GameModel.AboutToDisplayMap
                                        , tileWidth = model.tileWidth // 8
                                        , tileHeight = model.tileHeight // 8
                                        , viewport_width = model.viewport_width * 8
                                        , viewport_height = model.viewport_height * 8
                                    }

                                else
                                    model
                        in
                        ( newModel
                        , Cmd.batch [ Delay.after 200.0 Delay.Millisecond ShowMap ]
                        )

                    GameModel.ViewHideFog ->
                        let
                            newModel =
                                if model.currentDisplay == GameModel.DisplayRegularGame then
                                    { model | useFog = not model.useFog }

                                else
                                    model
                        in
                        ( newModel, Cmd.none )

                    GameModel.Nop ->
                        ( model, Cmd.none )

        ShowMap ->
            if model.currentDisplay == GameModel.AboutToDisplayMap then
                ( { model | currentDisplay = GameModel.DisplayMap }, Cmd.none )

            else
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
                                            -- consume immediatly the item which adds to the player health
                                            ( model.player.inventory
                                            , Grid.set pcoords (Tile.Floor { floorinfo | item = Nothing }) model.level
                                            , model.player.health + 4
                                            )

                                        _ ->
                                            ( Dict.update (Item.itemToString item) (\_ -> Just item) model.player.inventory
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
                    case Grid.get (GameModel.location x2 y2) model.level of
                        Just (Tile.Lever leverinfo) ->
                            if leverinfo.isUp then
                                model

                            else
                                { model | level = Grid.set (GameModel.location x2 y2) (Tile.Lever { leverinfo | isUp = True }) model.level }
                                    |> (\xmodel -> List.foldl (\cfunc modacc -> cfunc (GameModel.location x2 y2) modacc) xmodel (getModelChangerFuncs leverinfo.leverId model))

                        _ ->
                            model
                                |> (\model_ ->
                                        case Grid.get (GameModel.location x2 y2) model_.level of
                                            Just (Tile.ConverterTile initialTile newTile) ->
                                                { model_ | level = Grid.set (GameModel.location x2 y2) newTile model_.level }

                                            _ ->
                                                model_
                                   )

                mbFightChar =
                    case Dict.filter (\fcharId fightingCharacter -> fightingCharacter.floorId == model.currentFloorId && fightingCharacter.location == GameModel.location x2 y2) model.fightingCharacters |> Dict.values of
                        [] ->
                            Nothing

                        fightingCharacter :: es ->
                            Just fightingCharacter

                ( newModel2, themsg ) =
                    case mbFightChar of
                        Just fightingCharacter ->
                            if fightingCharacter.health > 0 && fightingCharacter.indexOfLight < fightingCharacter.indexOfLightMax && newModel.player.health > 0 then
                                ( newModel, StartOpponentInteraction fightingCharacter )

                            else if fightingCharacter.health <= 0 && fightingCharacter.playerCanWalkOverIfDead && (x_ /= 0 || y_ /= 0) then
                                ( { newModel | player = move ( x_, y_ ) newModel.level BeingsInTileGrid.isGridTileWalkable newModel.player }
                                    |> checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor
                                    |> openDoorIfPlayerStandingOnDoorAndClosed
                                    |> checkAndAlterViewportAnchorIfNecessary
                                , CleanUpAndFightingCharacterLogic
                                )

                            else
                                ( newModel, CleanUpAndFightingCharacterLogic )

                        Nothing ->
                            if x_ /= 0 || y_ /= 0 then
                                ( { newModel | player = move ( x_, y_ ) newModel.level BeingsInTileGrid.isGridTileWalkable newModel.player }
                                    |> checkIfPlayerStandingOnStairsOrHoleAndMoveToNewFloor
                                    |> openDoorIfPlayerStandingOnDoorAndClosed
                                    |> checkAndAlterViewportAnchorIfNecessary
                                , CleanUpAndFightingCharacterLogic
                                )

                            else
                                ( newModel, CleanUpAndFightingCharacterLogic )
            in
            update themsg newModel2

        CleanUpAndFightingCharacterLogic ->
            let
                ( newModel, lfightingCharacters ) =
                    model
                        |> cleanup
                        |> resetFightingCharacterMovesCurrentTurn
                        |> resetOtherCharacterMovesCurrentTurn
                        |> otherCharacters_AI
                        |> fightingCharacter_AI
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
                    , fightingCharacters = Dict.empty
                    , otherCharacters = Dict.empty
                  }
                    |> reveal
                , Cmd.none
                )

            else
                case lfightingCharacters |> List.head of
                    Just fightingCharacter_ ->
                        update (StartOpponentInteraction fightingCharacter_) newModel

                    Nothing ->
                        ( newModel |> reveal, cmdFillRandomIntsPool newModel )

        StartOpponentInteraction fightingCharacter ->
            let
                ( newThornsModel, thornsCmd ) =
                    ThornsUpdate.update (Thorns.Types.SetOpponentAndPlayerAndInitializeGrid fightingCharacter model.player) model.gameOfThornsModel

                {-
                   attackOutput =
                       attack model.player fightingCharacter model.pseudoRandomIntsPool

                   newModel =
                       log attackOutput.textMsg
                           { model
                               | player = attackOutput.dudeA
                               , fightingCharacters = Dict.insert fightingCharacter.id attackOutput.dudeB model.fightingCharacters --fightingCharacter_ :: getTailWithDefaultEmptyList model.fightingCharacters
                               , pseudoRandomIntsPool = attackOutput.randInts
                           }
                -}
                newModel =
                    { model
                        | currentDisplay = GameModel.DisplayGameOfThorns
                        , gameOfThornsModel = newThornsModel
                        , listeningToKeyInput = False
                    }

                newModel_after_cleanup =
                    --newModel |> cleanup |> resetFightingCharacterMovesCurrentTurn |> fightingCharacter_AI |> reveal
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

                newPlayer =
                    { oldPlayer | location = newLocation, placed = True }
            in
            case BeingsInTileGrid.isGridTileWalkable newLocation newPlayer model.level of
                True ->
                    ( { model
                        | player = newPlayer
                        , viewport_topleft_x = max 0 (newLocation.x - round (toFloat model.viewport_width / 2.0))
                        , viewport_topleft_y = max 0 (newLocation.y - round (toFloat model.viewport_height / 2))
                      }
                        |> reveal
                    , Cmd.none
                    )

                False ->
                    ( model, cmdGetRandomPositionedPlayer { oldPlayer | placed = False } gridBounds.minX gridBounds.maxX gridBounds.minY gridBounds.maxY )

        NewGridCoordinatesIndexToPlaceFightingCharacter fcharId idxPosition ->
            let
                mbActualFightChar =
                    Dict.get fcharId model.fightingCharacters

                mbFloorGrid =
                    case mbActualFightChar of
                        Nothing ->
                            Nothing

                        Just fchar ->
                            if fchar.floorId == model.currentFloorId then
                                Just model.level

                            else
                                Dict.get fchar.floorId model.floorDict
                                    |> Maybe.map .level

                gridCoordinates =
                    mbFloorGrid
                        |> Maybe.map (\g -> Grid.toCoordinates g)
                        |> Maybe.withDefault []
            in
            List.drop idxPosition gridCoordinates
                |> List.head
                |> Maybe.withDefault (Grid.Coordinate 1 1)
                |> (\coords -> update (NewRandomPointToPlaceFightingCharacter fcharId ( coords.x, coords.y )) model)

        NewRandomPointToPlaceFightingCharacter fcharId tupPosition ->
            let
                mbActualFightChar =
                    Dict.get fcharId model.fightingCharacters

                newLocation =
                    GameModel.location (Tuple.first tupPosition) (Tuple.second tupPosition)

                mbFloorGrid =
                    case mbActualFightChar of
                        Nothing ->
                            Nothing

                        Just fchar ->
                            if fchar.floorId == model.currentFloorId then
                                Just model.level

                            else
                                Dict.get fchar.floorId model.floorDict
                                    |> Maybe.map .level

                gridCoordinates =
                    mbFloorGrid
                        |> Maybe.map (\g -> Grid.toCoordinates g)
                        |> Maybe.withDefault []
            in
            case ( mbActualFightChar, mbFloorGrid ) of
                ( Just actualFightChar, Just actualFloorGrid ) ->
                    case BeingsInTileGrid.isGridTileWalkable newLocation actualFightChar actualFloorGrid of
                        True ->
                            ( { model | fightingCharacters = GameModel.placeExistingFightingCharacter fcharId newLocation model.fightingCharacters }, Cmd.none )

                        False ->
                            ( model, cmdGetRandomPositionedFightingCharacter actualFightChar fcharId (gridCoordinates |> List.length) )

                _ ->
                    ( model, Cmd.none )

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

            else if strCharacter == "fightingCharacter" then
                let
                    newModel =
                        GameModel.mbUpdateFightingCharacterInitiativeByMbFCharId intval mbCharacterId model
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

                genOutputRecord =
                    --{ tileGrid , lroomRectangles , ltunnelRectangles , unusedRandoms  }
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
                            , viewport_width = cFloor.viewport_width
                            , viewport_height = cFloor.viewport_height
                            , total_width = cFloor.total_width
                            , total_height = cFloor.total_height
                            , mapImgStr = cFloor.mapImgStr
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
            move ( delta_x, delta_y ) newModel.level BeingsInTileGrid.isGridTileWalkable newModel.player
    in
    { newModel
        | player = player_
        , viewport_topleft_x = max 0 (Tuple.first locTuple - round (toFloat newModel.viewport_width / 2.0))
        , viewport_topleft_y = max 0 (Tuple.second locTuple - round (toFloat newModel.viewport_height / 2))
    }
        |> reveal


position_viewport_in_order_to_center_player : Model -> Model
position_viewport_in_order_to_center_player model =
    { model
        | viewport_topleft_x = max 0 (model.player.location.x - round (toFloat model.viewport_width / 2.0))
        , viewport_topleft_y = max 0 (model.player.location.y - round (toFloat model.viewport_height / 2))
    }
        |> reveal


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


checkAndAlterViewportAnchorIfNecessary : Model -> Model
checkAndAlterViewportAnchorIfNecessary model =
    let
        p_x_dist =
            5

        newXanchor =
            if model.player.location.x <= model.viewport_topleft_x then
                max 0 (model.viewport_topleft_x - (model.viewport_width - p_x_dist))

            else if model.player.location.x >= (model.viewport_topleft_x + (model.viewport_width - 1)) then
                min (model.viewport_topleft_x + (model.viewport_width - p_x_dist)) (model.total_width - (model.viewport_width - p_x_dist))

            else
                model.viewport_topleft_x

        p_y_dist =
            5

        newYanchor =
            if model.player.location.y <= model.viewport_topleft_y then
                max 0 (model.viewport_topleft_y - (model.viewport_height - p_y_dist))

            else if model.player.location.y >= (model.viewport_topleft_y + (model.viewport_height - 1)) then
                min (model.viewport_topleft_y + (model.viewport_height - p_y_dist)) (model.total_height - (model.viewport_height - p_y_dist))

            else
                model.viewport_topleft_y
    in
    { model | viewport_topleft_x = newXanchor, viewport_topleft_y = newYanchor }


cmdGetRandomPositionedPlayer : Player -> Int -> Int -> Int -> Int -> Cmd Msg
cmdGetRandomPositionedPlayer player minX maxX minY maxY =
    if player.placed then
        Cmd.none

    else
        Random.generate NewRandomPointToPlacePlayer (getRandIntPair minX maxX minY maxY)


cmdGetRandomPositionedFightingCharacter : FightingCharacter -> FightingCharacterId -> Int -> Cmd Msg
cmdGetRandomPositionedFightingCharacter actualFightChar fcharId maxX =
    if actualFightChar.placed then
        Cmd.none

    else
        Random.generate (NewGridCoordinatesIndexToPlaceFightingCharacter fcharId) (getRandInt 0 (maxX - 1))


cmdGenerateRandomInitiativeValue : String -> Maybe FightingCharacterId -> Int -> Int -> Cmd Msg
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


getRandInt : Int -> Int -> Random.Generator Int
getRandInt minX maxX =
    Random.int minX maxX


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


move :
    ( Int, Int )
    -> Grid.Grid Tile
    ->
        (GameModel.Location
         -> { a | location : GameModel.Location, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, canMoveThroughHoles : Bool, canUseTeleporters : Bool, inventory : Beings.Inventory, initiative : Int }
         -> Grid.Grid Tile
         -> Bool
        )
    -> { a | location : GameModel.Location, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, canMoveThroughHoles : Bool, canUseTeleporters : Bool, inventory : Beings.Inventory, initiative : Int }
    -> { a | location : GameModel.Location, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, canMoveThroughHoles : Bool, canUseTeleporters : Bool, inventory : Beings.Inventory, initiative : Int }
move ( x_shift, y_shift ) grid isWalkableFunc a =
    BeingsInTileGrid.move ( x_shift, y_shift ) grid isWalkableFunc a


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


resetOtherCharacterMovesCurrentTurn : Model -> Model
resetOtherCharacterMovesCurrentTurn model =
    let
        updatedOtherCharacters =
            Dict.map (\ocharId otherCharacter -> { otherCharacter | nrMovesInCurrentTurn = 0 }) model.otherCharacters
    in
    { model | otherCharacters = updatedOtherCharacters }


resetFightingCharacterMovesCurrentTurn : Model -> Model
resetFightingCharacterMovesCurrentTurn model =
    let
        updatedFightCharacters =
            Dict.map (\fcharId fightingCharacter -> { fightingCharacter | nrMovesInCurrentTurn = 0 }) model.fightingCharacters
    in
    { model | fightingCharacters = updatedFightCharacters }


increseNrOfFightingCharacterMovesInCurrentTurn : FightingCharacterId -> Model -> Model
increseNrOfFightingCharacterMovesInCurrentTurn fcharId model =
    let
        updatedFightingCharaters =
            Dict.update fcharId (\mbfchar -> mbfchar |> Maybe.map (\fchar -> { fchar | nrMovesInCurrentTurn = fchar.nrMovesInCurrentTurn + 1 })) model.fightingCharacters
    in
    { model | fightingCharacters = updatedFightingCharaters }


cleanup : Model -> Model
cleanup model =
    let
        dead_and_disappears =
            Dict.filter (\fcharId fightingCharacter -> fightingCharacter.health <= 0 && fightingCharacter.disappearsWhenHealthIsZero) model.fightingCharacters

        dead_and_doesnt_disappear =
            Dict.filter (\fcharId fightingCharacter -> fightingCharacter.health <= 0 && not fightingCharacter.disappearsWhenHealthIsZero) model.fightingCharacters

        alive_no_enlightenment =
            Dict.filter (\fcharId fightingCharacter -> fightingCharacter.health > 0 && fightingCharacter.indexOfLight < fightingCharacter.indexOfLightMax) model.fightingCharacters

        alive_enlightened_disappears =
            Dict.filter (\fcharId fightingCharacter -> fightingCharacter.health > 0 && fightingCharacter.indexOfLight >= fightingCharacter.indexOfLightMax && fightingCharacter.disappearsWhenIndexOfLightMax) model.fightingCharacters

        alive_enlightened_doesnt_disappear =
            Dict.filter (\fcharId fightingCharacter -> fightingCharacter.health > 0 && fightingCharacter.indexOfLight >= fightingCharacter.indexOfLightMax && not fightingCharacter.disappearsWhenIndexOfLightMax) model.fightingCharacters

        keys_to_remove =
            (dead_and_disappears |> Dict.keys) ++ (alive_enlightened_disappears |> Dict.keys)

        msg =
            if Dict.size dead_and_disappears == 0 then
                Nothing

            else
                Just (Dict.foldl (\id nstr acc -> acc ++ nstr) "" <| Dict.map (\fcharId fightingCharacter -> fightingCharacter.name ++ " died. ") dead_and_disappears)

        newModel =
            { model | fightingCharacters = Dict.filter (\k v -> not (List.member k keys_to_remove)) model.fightingCharacters }
    in
    case msg of
        Nothing ->
            newModel

        Just m ->
            log m newModel


fightingCharacterExceedsNrMovesInCurrentTurn : FightingCharacterId -> Model -> Bool
fightingCharacterExceedsNrMovesInCurrentTurn fcharId model =
    let
        mbFightChar =
            Dict.get fcharId model.fightingCharacters
    in
    case mbFightChar of
        Nothing ->
            True

        Just fightingCharacter ->
            fightingCharacter.nrMovesInCurrentTurn >= fightingCharacter.maxNrCharacterMovesPerTurn


fightingCharacter_AI : Model -> ( Model, List FightingCharacter )
fightingCharacter_AI model =
    let
        fightingCharactersPlayerRec =
            FightingCharacterInTileGrid.fightingCharacter_AI model.currentDisplay model.currentFloorId (FightingCharacterInTileGrid.OpponentsAndPlayerRec model.fightingCharacters model.player model.level model.floorDict [] [] model.pseudoRandomIntsPool)

        newModel =
            { model
                | fightingCharacters = fightingCharactersPlayerRec.fightingCharacters
                , player = fightingCharactersPlayerRec.player
                , level = fightingCharactersPlayerRec.grid
                , floorDict = fightingCharactersPlayerRec.floorDict
                , pseudoRandomIntsPool = fightingCharactersPlayerRec.lrandInts
            }

        lfightingCharactersForGoT =
            fightingCharactersPlayerRec.lFightingCharactersForGameOfThorns
    in
    ( newModel, lfightingCharactersForGoT )


otherCharacters_AI : Model -> Model
otherCharacters_AI model =
    let
        otherCharactersPlayerRec =
            OtherCharacterInTileGrid.otherCharacter_AI model.currentDisplay model.currentFloorId (OtherCharacterInTileGrid.OthersAndPlayerRec model.otherCharacters model.player model.level model.floorDict [] model.pseudoRandomIntsPool)

        newModel =
            { model
                | otherCharacters = otherCharactersPlayerRec.otherCharacters
                , player = otherCharactersPlayerRec.player
                , level = otherCharactersPlayerRec.grid
                , floorDict = otherCharactersPlayerRec.floorDict
                , pseudoRandomIntsPool = otherCharactersPlayerRec.lrandInts
            }
    in
    newModel



-- Right now this just reveals a box around the player


reveal : Model -> Model
reveal model =
    let
        intermediateModelGrid =
            Grid.map
                (\t ->
                    if Tile.getTileVisibility t == Visible then
                        Tile.setTileVisibility Explored t

                    else
                        t
                )
                model.level

        intermediateModel =
            { model | level = intermediateModelGrid }

        newModel =
            List.foldl (\loc imodel -> GameModel.setModelTileVisibility loc Visible imodel) intermediateModel (GameModel.visible intermediateModel)
    in
    newModel
