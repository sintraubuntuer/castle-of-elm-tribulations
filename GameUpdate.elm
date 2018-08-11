module GameUpdate exposing (..)

--import Generator
--import Collage

import Dict exposing (Dict)
import GameModel
import Grid
import MapGen
import Random


log : String -> GameModel.State -> GameModel.State
log s state =
    { state | log = s :: state.log }


getTailWithDefaultEmptyList : List a -> List a
getTailWithDefaultEmptyList la =
    List.tail la
        |> Maybe.withDefault []


type Msg
    = Noop
    | KeyDown GameModel.Input
    | NewRandomPointToPlacePlayer ( Int, Int )
    | NewRandomPointToPlaceEnemy GameModel.EnemyId ( Int, Int )
    | NewRandomFloatsForGenCave (List Float)
    | RandomInitiativeValue String (Maybe GameModel.EnemyId) Int
    | NewRandomIntsAddToPool (List Int)
    | NewRandomIntsAddToPoolAndGenerateRandomMap (List Int)


update : Msg -> GameModel.State -> ( GameModel.State, Cmd Msg )
update msg state =
    case msg of
        Noop ->
            ( state, Cmd.none )

        KeyDown input ->
            let
                player =
                    state.player

                { x, y } =
                    player.location

                ( x_, y_ ) =
                    case input of
                        GameModel.Up ->
                            ( 0, 0 - 1 )

                        GameModel.Down ->
                            ( 0, 0 + 1 )

                        GameModel.Left ->
                            ( 0 - 1, 0 )

                        GameModel.Right ->
                            ( 0 + 1, 0 )

                        GameModel.Nop ->
                            ( 0, 0 )

                ( x2, y2 ) =
                    ( x + x_, y + y_ )

                mbEnemy =
                    case Dict.filter (\enemyid enemy -> enemy.location == GameModel.location x2 y2) state.enemies |> Dict.values of
                        [] ->
                            Nothing

                        enemy :: es ->
                            Just enemy

                newState =
                    case mbEnemy of
                        Just enemy ->
                            let
                                ( player_, enemy_, msg, newprandInts ) =
                                    attack player enemy state.pseudoRandomIntsPool
                            in
                            log msg
                                { state
                                    | player = player_
                                    , enemies = Dict.insert enemy.id enemy_ state.enemies --enemy_ :: getTailWithDefaultEmptyList state.enemies
                                    , pseudoRandomIntsPool = newprandInts
                                }

                        Nothing ->
                            if x_ /= 0 || y_ /= 0 then
                                { state | player = move ( x_, y_ ) state player }
                                    |> checkAndAlterDisplayAnchorIfNecessary
                            else
                                state

                newState2 =
                    newState |> cleanup |> resetEnemyMovesCurrentTurn |> ai |> reveal
            in
            ( newState2, cmdFillRandomIntsPool newState2 )

        NewRandomPointToPlacePlayer tupPosition ->
            let
                oldPlayer =
                    state.player

                newLocation =
                    GameModel.location (Tuple.first tupPosition) (Tuple.second tupPosition)

                gridBounds =
                    Grid.getGridBoundsToPlacePlayer state.level

                newPlayer =
                    { oldPlayer | location = newLocation, placed = True }
            in
            case GameModel.isModelTileWalkable newLocation state of
                True ->
                    ( { state | player = newPlayer, x_display_anchor = max 0 (newLocation.x - round (toFloat state.window_width / 2.0)), y_display_anchor = max 0 (newLocation.y - round (toFloat state.window_height / 2)) }
                        |> reveal
                    , Cmd.none
                    )

                False ->
                    ( state, cmdGetRandomPositionedPlayer oldPlayer gridBounds.minX gridBounds.maxX gridBounds.minY gridBounds.maxY )

        NewRandomPointToPlaceEnemy enemyId tupPosition ->
            let
                mbActualEnemy =
                    Dict.get enemyId state.enemies

                newLocation =
                    GameModel.location (Tuple.first tupPosition) (Tuple.second tupPosition)

                gridBounds =
                    Grid.getGridBoundsToPlaceEnemy state.level

                mbNewEnemy =
                    case mbActualEnemy of
                        Just actualEnemy ->
                            Just { actualEnemy | location = newLocation, placed = True }

                        Nothing ->
                            Nothing
            in
            case mbActualEnemy of
                Nothing ->
                    ( state, Cmd.none )

                Just actualEnemy ->
                    case GameModel.isModelTileWalkable newLocation state of
                        True ->
                            ( { state | enemies = GameModel.placeExistingEnemy enemyId newLocation state.enemies }, Cmd.none )

                        False ->
                            --( { state | player = newPlayer }, Cmd.none )
                            ( state, cmdGetRandomPositionedEnemy actualEnemy enemyId gridBounds.minX gridBounds.maxX gridBounds.minY gridBounds.maxY )

        --randomlyPlaceExistingEnemies : List ( Location, EnemyId ) -> State -> State
        NewRandomFloatsForGenCave lfloats ->
            let
                theSize =
                    state.level.size

                newGrid =
                    MapGen.randomCave ( theSize.width, theSize.height ) lfloats
            in
            ( { state | level = newGrid }, Cmd.none )

        RandomInitiativeValue strCharacter mbCharacterId intval ->
            if strCharacter == "player" then
                let
                    oldPlayer =
                        state.player

                    newPlayer =
                        { oldPlayer | initiative = intval }
                in
                ( { state | player = newPlayer }, Cmd.none )
            else if strCharacter == "enemy" then
                let
                    newState =
                        GameModel.mbUpdateEnemyInitiativeByMbEnemyId intval mbCharacterId state
                in
                ( newState, Cmd.none )
            else
                ( state, Cmd.none )

        NewRandomIntsAddToPool lints ->
            ( { state | pseudoRandomIntsPool = lints ++ state.pseudoRandomIntsPool }
            , Cmd.none
            )

        NewRandomIntsAddToPoolAndGenerateRandomMap lints ->
            let
                ( newGrid, lrectangles, ltunnelrectangles, unused_prand_lints ) =
                    MapGen.randomMapGeneratorWithRooms state.total_width state.total_height state.roomsInfo.maxNrOfRooms state.roomsInfo.maxRoomSize state.roomsInfo.minRoomSize lints state.level

                newstate =
                    { state | level = newGrid, pseudoRandomIntsPool = unused_prand_lints }
            in
            ( newstate, cmdFillRandomIntsPool newstate )


checkAndAlterDisplayAnchorIfNecessary : GameModel.State -> GameModel.State
checkAndAlterDisplayAnchorIfNecessary state =
    let
        newXanchor =
            if state.player.location.x <= state.x_display_anchor then
                max 0 (state.x_display_anchor - (state.window_width - 2))
            else if state.player.location.x >= (state.x_display_anchor + (state.window_width - 1)) then
                min (state.x_display_anchor + (state.window_width - 2)) (state.total_width - 1)
            else
                state.x_display_anchor

        newYanchor =
            if state.player.location.y <= state.y_display_anchor then
                max 0 (state.y_display_anchor - (state.window_height - 2))
            else if state.player.location.y >= (state.y_display_anchor + (state.window_height - 1)) then
                min (state.y_display_anchor + (state.window_height - 2)) (state.total_height - 1)
            else
                state.y_display_anchor
    in
    { state | x_display_anchor = newXanchor, y_display_anchor = newYanchor }


cmdGetRandomPositionedPlayer : GameModel.Player -> Int -> Int -> Int -> Int -> Cmd Msg
cmdGetRandomPositionedPlayer player minX maxX minY maxY =
    if player.placed then
        Cmd.none
    else
        Random.generate NewRandomPointToPlacePlayer (getRandIntPair minX maxX minY maxY)


cmdGetRandomPositionedEnemy : GameModel.Enemy -> GameModel.EnemyId -> Int -> Int -> Int -> Int -> Cmd Msg
cmdGetRandomPositionedEnemy actualEnemy enemyId minX maxX minY maxY =
    if actualEnemy.placed then
        Cmd.none
    else
        Random.generate (NewRandomPointToPlaceEnemy enemyId) (getRandIntPair minX maxX minY maxY)


cmdGenerateRandomInitiativeValue : String -> Maybe GameModel.EnemyId -> Int -> Int -> Cmd Msg
cmdGenerateRandomInitiativeValue strCharacter mbCharacterId minval maxval =
    Random.generate (RandomInitiativeValue strCharacter mbCharacterId) (Random.int minval maxval)


cmdFillRandomIntsPool : GameModel.State -> Cmd Msg
cmdFillRandomIntsPool state =
    let
        nrToAdd =
            500 - List.length state.pseudoRandomIntsPool
    in
    if nrToAdd > 0 then
        Random.generate NewRandomIntsAddToPool (Random.list nrToAdd (Random.int 1 100))
    else
        Cmd.none


cmdFillRandomIntsPoolAndGenerateRandomMap : GameModel.State -> Cmd Msg
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


move : ( Int, Int ) -> GameModel.State -> { a | location : GameModel.Location, initiative : Int } -> { a | location : GameModel.Location, initiative : Int }
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
    -> ( { a | coordination : Int, power : Int, initiative : Int, name : String }, { b | stealth : Int, protection : Int, armor : Int, health : Int, name : String }, String, List Int )
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
                dude2.protection - (dude1.coordination - rem dude2.stealth 100)
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
                dude1.name ++ " hit " ++ dude2.name ++ " for " ++ toString dmg ++ " dmg"
    in
    ( { dude1 | initiative = dude1.initiative + 100 }, { dude2 | health = result }, msg, newprandInts2 )


resetEnemyMovesCurrentTurn : GameModel.State -> GameModel.State
resetEnemyMovesCurrentTurn state =
    let
        newEnemies =
            Dict.map (\enemyid enemy -> { enemy | nrMovesInCurrentTurn = 0 }) state.enemies
    in
    { state | enemies = newEnemies }


increseNrOfEnemyMovesInCurrentTurn : GameModel.EnemyId -> GameModel.State -> GameModel.State
increseNrOfEnemyMovesInCurrentTurn enemyid state =
    let
        newEnemies =
            Dict.update enemyid (\mbenemy -> mbenemy |> Maybe.map (\en -> { en | nrMovesInCurrentTurn = en.nrMovesInCurrentTurn + 1 })) state.enemies
    in
    { state | enemies = newEnemies }


cleanup : GameModel.State -> GameModel.State
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


ai : GameModel.State -> GameModel.State
ai state =
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
            ai state2

        Nothing ->
            state


attackIfClose : GameModel.Enemy -> GameModel.State -> GameModel.State
attackIfClose enemy state =
    case List.filter (\location -> location == state.player.location) (Grid.neighborhoodCalc 1 enemy.location) of
        location :: locs ->
            let
                ( enemy_, player_, msg, newprandInts ) =
                    attack enemy state.player state.pseudoRandomIntsPool
            in
            log msg
                { state
                    | player = player_
                    , enemies = Dict.insert enemy.id enemy_ state.enemies -- enemy_ :: getTailWithDefaultEmptyList state.enemies
                    , pseudoRandomIntsPool = newprandInts
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


reveal : GameModel.State -> GameModel.State
reveal state =
    let
        intermediateStateGrid =
            Grid.map
                (\t ->
                    if GameModel.getTileVisibility t == GameModel.Visible then
                        GameModel.setTileVisibility GameModel.Explored t
                    else
                        t
                )
                state.level

        intermediateState =
            { state | level = intermediateStateGrid }

        newState =
            List.foldl (\loc istate -> GameModel.setModelTileVisibility loc GameModel.Visible istate) intermediateState (GameModel.visible state)
    in
    newState



{- }
   -- Right now this just reveals a box around the player


   reveal : GameModel.State -> GameModel.State
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
