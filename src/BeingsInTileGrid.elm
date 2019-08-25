module BeingsInTileGrid exposing
    ( attack
    , attackIfClose_OtherwiseMove
    , enemyMove
    , enemy_AI
    , isGridTileWalkable
    , isTileWalkable
    , move
    )

import Beings
import Dict exposing (Dict)
import GameModel exposing (CurrentDisplay(..))
import Grid
import Tile exposing (Tile(..))


move : ( Int, Int ) -> Grid.Grid Tile -> (Grid.Coordinate -> { a | location : Grid.Coordinate, direction : Beings.Direction, inventory : Beings.Inventory, initiative : Int } -> Grid.Grid Tile -> Bool) -> { a | location : Grid.Coordinate, direction : Beings.Direction, inventory : Beings.Inventory, initiative : Int } -> { a | location : Grid.Coordinate, direction : Beings.Direction, inventory : Beings.Inventory, initiative : Int }
move ( x_shift, y_shift ) grid isWalkableFunc a =
    let
        location =
            Grid.Coordinate (a.location.x + x_shift) (a.location.y + y_shift)

        initiative =
            a.initiative + 100
    in
    --case GameModel.isModelTileWalkable location a model of
    case isWalkableFunc location a grid of
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


isGridTileWalkable : Grid.Coordinate -> { a | inventory : Beings.Inventory } -> Grid.Grid Tile -> Bool
isGridTileWalkable location_ being grid =
    Grid.get location_ grid
        |> Maybe.map (isTileWalkable being)
        |> Maybe.withDefault False


isTileWalkable : { a | inventory : Beings.Inventory } -> Tile -> Bool
isTileWalkable being tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isWalkable

        Grass grassinfo ->
            grassinfo.isWalkable

        Tree treeInfo ->
            False

        Stairs sinfo ->
            True

        Hole hinfo ->
            True

        Wall wInfo ->
            case wInfo.mbTeleporterObject of
                Just tel ->
                    True

                Nothing ->
                    False

        WallOver wOverInfo ->
            False

        Door doorinfo ->
            --doorinfo.isOpen || List.contains doorinfo.requiresToOpen (Dict.values player.inventory)
            List.foldl (\it bacc -> inList it (Dict.values being.inventory) && bacc) True doorinfo.requiresToOpen

        Lever leverInfo ->
            False

        Flag flagInfo ->
            False

        Column columnInfo ->
            False

        Water waterInfo ->
            waterInfo.isWalkable

        ConverterTile it ct ->
            False

        NoTileYet ->
            False


type alias EnemiesPlayerRec =
    { enemies : Dict Beings.EnemyId Beings.Enemy
    , player : Beings.Player
    , grid : Grid.Grid Tile
    , lEnemiesForGameOfThorns : List Beings.Enemy
    , textMsgs : List String
    , lrandInts : List Int
    }


increseNrOfEnemyMovesInCurrentTurn : Beings.EnemyId -> EnemiesPlayerRec -> EnemiesPlayerRec
increseNrOfEnemyMovesInCurrentTurn enemyid enemiesPlayerRec =
    let
        updatedEnemies =
            Dict.update enemyid (\mbenemy -> mbenemy |> Maybe.map (\en -> { en | nrMovesInCurrentTurn = en.nrMovesInCurrentTurn + 1 })) enemiesPlayerRec.enemies
    in
    { enemiesPlayerRec | enemies = updatedEnemies }


enemyExceedsNrMovesInCurrentTurn : Beings.EnemyId -> EnemiesPlayerRec -> Bool
enemyExceedsNrMovesInCurrentTurn enemyid enemiesPlayerRec =
    let
        mbEnemy =
            Dict.get enemyid enemiesPlayerRec.enemies
    in
    case mbEnemy of
        Nothing ->
            True

        Just enemy ->
            enemy.nrMovesInCurrentTurn >= enemy.maxNrEnemyMovesPerTurn


enemy_AI : CurrentDisplay -> Int -> EnemiesPlayerRec -> EnemiesPlayerRec
enemy_AI currentDisplay currentFloorId enemiesPlayerRec =
    let
        enemyIdEnemyPairList =
            Dict.filter (\enemyid enemy -> enemy.health > 0 && enemy.nrMovesInCurrentTurn < enemy.maxNrEnemyMovesPerTurn) enemiesPlayerRec.enemies
                |> Dict.toList
    in
    List.foldl (\enpair modelacc -> ai_helper_func currentDisplay currentFloorId (Tuple.first enpair) modelacc) enemiesPlayerRec enemyIdEnemyPairList


ai_helper_func : CurrentDisplay -> Int -> Beings.EnemyId -> EnemiesPlayerRec -> EnemiesPlayerRec
ai_helper_func currentDisplay currentFloorId enemyid enemies_player_rec =
    if enemyExceedsNrMovesInCurrentTurn enemyid enemies_player_rec || currentDisplay == DisplayGameOfThorns then
        enemies_player_rec

    else
        let
            mbenemy =
                Dict.get enemyid enemies_player_rec.enemies

            ( enemies_player_rec2, mbenemyForGameOfThorns ) =
                case mbenemy of
                    Nothing ->
                        ( enemies_player_rec, Nothing )

                    Just enemy ->
                        if enemy.floorId == currentFloorId && enemy.indexOfLight < enemy.indexOfLightMax && enemies_player_rec.player.health > 0 && currentDisplay /= DisplayGameOfThorns then
                            let
                                outRec =
                                    attackIfClose_OtherwiseMove enemy enemies_player_rec.player currentFloorId enemies_player_rec.grid enemies_player_rec.lrandInts

                                newEnPlayerRec =
                                    ( { enemies_player_rec
                                        | enemies = Dict.update enemyid (\_ -> Just outRec.enemy) enemies_player_rec.enemies
                                        , player = outRec.player
                                        , lEnemiesForGameOfThorns = enemies_player_rec.lEnemiesForGameOfThorns ++ (outRec.mbEnemyForGameOfThorns |> Maybe.map (\x -> [ x ]) |> Maybe.withDefault [])
                                        , textMsgs = []
                                        , lrandInts = outRec.lrandInts
                                      }
                                    , outRec.mbEnemyForGameOfThorns
                                    )
                                        |> (\( x, y ) -> ( increseNrOfEnemyMovesInCurrentTurn enemyid x, y ))
                            in
                            newEnPlayerRec

                        else
                            ( enemyMove enemy enemies_player_rec.player.location enemies_player_rec.grid enemies_player_rec.lrandInts
                                |> (\( enem, lrand ) -> { enemies_player_rec | enemies = Dict.update enemyid (\_ -> Just enem) enemies_player_rec.enemies, lrandInts = lrand })
                                |> (\x -> increseNrOfEnemyMovesInCurrentTurn enemyid x)
                            , Nothing
                            )
        in
        case mbenemyForGameOfThorns of
            Just en ->
                { enemies_player_rec2 | lEnemiesForGameOfThorns = enemies_player_rec.lEnemiesForGameOfThorns ++ [ en ] }

            Nothing ->
                ai_helper_func currentDisplay currentFloorId enemyid enemies_player_rec2


attackIfClose_OtherwiseMove : Beings.Enemy -> Beings.Player -> Int -> Grid.Grid Tile -> List Int -> { enemy : Beings.Enemy, player : Beings.Player, mbEnemyForGameOfThorns : Maybe Beings.Enemy, textMsg : String, lrandInts : List Int }
attackIfClose_OtherwiseMove enemy player currentFloorId grid pseudoRandomIntsPool =
    if enemy.floorId /= currentFloorId && enemy.health > 0 then
        let
            ( updatedEnemy, updatedRandInts ) =
                enemyMove enemy player.location grid pseudoRandomIntsPool
        in
        { enemy = updatedEnemy, player = player, mbEnemyForGameOfThorns = Nothing, textMsg = "", lrandInts = updatedRandInts }

    else
        case List.filter (\location -> location == player.location) (Grid.neighborhoodCalc 1 enemy.location) of
            location :: locs ->
                if enemy.attacksUsingGameOfThorns then
                    { enemy = enemy, player = player, mbEnemyForGameOfThorns = Just enemy, textMsg = "", lrandInts = pseudoRandomIntsPool }

                else
                    let
                        attackOutput =
                            attack enemy player pseudoRandomIntsPool
                    in
                    { enemy = attackOutput.dudeA
                    , player = attackOutput.dudeB
                    , mbEnemyForGameOfThorns = Nothing
                    , textMsg = attackOutput.textMsg
                    , lrandInts = attackOutput.randInts
                    }

            [] ->
                let
                    ( updatedEnemy, updatedRandInts ) =
                        enemyMove enemy player.location grid pseudoRandomIntsPool
                in
                { enemy = updatedEnemy, player = player, mbEnemyForGameOfThorns = Nothing, textMsg = "", lrandInts = updatedRandInts }


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


enemyMove : Beings.Enemy -> Grid.Coordinate -> Grid.Grid Tile -> List Int -> ( Beings.Enemy, List Int )
enemyMove enemy player_location grid lRandomInts =
    let
        ( xrand, yrand, updatedRandInts ) =
            ( List.head lRandomInts |> Maybe.withDefault 0
            , List.drop 1 lRandomInts
                |> List.head
                |> Maybe.withDefault 0
            , List.drop 2 lRandomInts
            )

        x_delta_toPlayer =
            player_location.x - enemy.location.x

        y_delta_toPlayer =
            player_location.y - enemy.location.y

        xscaled =
            if x_delta_toPlayer > 0 then
                if xrand <= 85 then
                    -- 85% probability
                    1

                else
                    -1

            else if x_delta_toPlayer < 0 then
                if xrand <= 85 then
                    -- 85% probability
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
            move ( xscaled, yscaled ) grid isGridTileWalkable enemy
                |> (\en ->
                        if en.location == player_location then
                            enemy

                        else
                            en
                   )
    in
    ( enemy_, updatedRandInts )


inList : a -> List a -> Bool
inList a_val la =
    List.filter (\elem -> elem == a_val) la
        |> List.length
        |> (\x -> x > 0)
