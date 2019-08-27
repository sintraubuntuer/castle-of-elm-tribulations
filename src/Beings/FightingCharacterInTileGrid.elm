module Beings.FightingCharacterInTileGrid exposing
    ( OpponentsAndPlayerRec
    , attack
    , attackIfClose_OtherwiseMove
    , fightingCharacterMove
    , fightingCharacter_AI
    )

import Beings.Beings as Beings
import Beings.BeingsInTileGrid as BeingsInTileGrid
    exposing
        ( isGridTileWalkable
        , isTileWalkable
        , move
        )
import Dict exposing (Dict)
import GameModel exposing (CurrentDisplay(..), FloorStore)
import Grid
import Tile exposing (Tile(..))


type alias OpponentsAndPlayerRec =
    { fightingCharacters : Dict Beings.FightingCharacterId Beings.FightingCharacter
    , player : Beings.Player
    , grid : Grid.Grid Tile
    , floorDict : Dict Int GameModel.FloorStore
    , lFightingCharactersForGameOfThorns : List Beings.FightingCharacter
    , textMsgs : List String
    , lrandInts : List Int
    }


increseNrOfFightingCharacterMovesInCurrentTurn : Beings.FightingCharacterId -> OpponentsAndPlayerRec -> OpponentsAndPlayerRec
increseNrOfFightingCharacterMovesInCurrentTurn fcharId opponentAndPlayerRec =
    let
        updatedFightingCharacters =
            Dict.update fcharId (\mbFightCharacter -> mbFightCharacter |> Maybe.map (\fchar -> { fchar | nrMovesInCurrentTurn = fchar.nrMovesInCurrentTurn + 1 })) opponentAndPlayerRec.fightingCharacters
    in
    { opponentAndPlayerRec | fightingCharacters = updatedFightingCharacters }


fightingCharacterExceedsNrMovesInCurrentTurn : Beings.FightingCharacterId -> OpponentsAndPlayerRec -> Bool
fightingCharacterExceedsNrMovesInCurrentTurn fcharId opponentAndPlayerRec =
    let
        mbFightingCharacter =
            Dict.get fcharId opponentAndPlayerRec.fightingCharacters
    in
    case mbFightingCharacter of
        Nothing ->
            True

        Just fightingCharacter ->
            fightingCharacter.nrMovesInCurrentTurn >= fightingCharacter.maxNrCharacterMovesPerTurn


fightingCharacter_AI : CurrentDisplay -> Int -> OpponentsAndPlayerRec -> OpponentsAndPlayerRec
fightingCharacter_AI currentDisplay currentFloorId opponentAndPlayerRec =
    let
        fcIdFightCharacterPairList =
            Dict.filter (\fcharId fightingCharacter -> fightingCharacter.health > 0 && fightingCharacter.nrMovesInCurrentTurn < fightingCharacter.maxNrCharacterMovesPerTurn) opponentAndPlayerRec.fightingCharacters
                |> Dict.toList
    in
    List.foldl (\enpair modelacc -> ai_helper_func currentDisplay currentFloorId (Tuple.first enpair) modelacc) opponentAndPlayerRec fcIdFightCharacterPairList


ai_helper_func : CurrentDisplay -> Int -> Beings.FightingCharacterId -> OpponentsAndPlayerRec -> OpponentsAndPlayerRec
ai_helper_func currentDisplay currentFloorId fcharId opponents_and_player_rec =
    if fightingCharacterExceedsNrMovesInCurrentTurn fcharId opponents_and_player_rec || currentDisplay == DisplayGameOfThorns then
        opponents_and_player_rec

    else
        let
            mbFightCharacter =
                Dict.get fcharId opponents_and_player_rec.fightingCharacters

            ( opponents_and_player_rec2, mbFightCharacterForGameOfThorns ) =
                case mbFightCharacter of
                    Nothing ->
                        ( opponents_and_player_rec, Nothing )

                    Just fightingCharacter ->
                        if fightingCharacter.floorId == currentFloorId && fightingCharacter.indexOfLight < fightingCharacter.indexOfLightMax && opponents_and_player_rec.player.health > 0 && currentDisplay /= DisplayGameOfThorns then
                            let
                                outRec =
                                    attackIfClose_OtherwiseMove fightingCharacter opponents_and_player_rec.player currentFloorId opponents_and_player_rec.grid opponents_and_player_rec.floorDict opponents_and_player_rec.lrandInts

                                newEnPlayerRec =
                                    ( { opponents_and_player_rec
                                        | fightingCharacters = Dict.update fcharId (\_ -> Just outRec.fightingCharacter) opponents_and_player_rec.fightingCharacters
                                        , player = outRec.player
                                        , lFightingCharactersForGameOfThorns = opponents_and_player_rec.lFightingCharactersForGameOfThorns ++ (outRec.mbFightingCharacterForGameOfThorns |> Maybe.map (\x -> [ x ]) |> Maybe.withDefault [])
                                        , textMsgs = []
                                        , lrandInts = outRec.lrandInts
                                      }
                                    , outRec.mbFightingCharacterForGameOfThorns
                                    )
                                        |> (\( x, y ) -> ( increseNrOfFightingCharacterMovesInCurrentTurn fcharId x, y ))
                            in
                            newEnPlayerRec

                        else
                            ( fightingCharacterMove fightingCharacter opponents_and_player_rec.player currentFloorId opponents_and_player_rec.grid opponents_and_player_rec.floorDict opponents_and_player_rec.lrandInts
                                |> (\( fchar, lrand ) -> { opponents_and_player_rec | fightingCharacters = Dict.update fcharId (\_ -> Just fchar) opponents_and_player_rec.fightingCharacters, lrandInts = lrand })
                                |> (\x -> increseNrOfFightingCharacterMovesInCurrentTurn fcharId x)
                            , Nothing
                            )
        in
        case mbFightCharacterForGameOfThorns of
            Just fchar ->
                { opponents_and_player_rec2 | lFightingCharactersForGameOfThorns = opponents_and_player_rec.lFightingCharactersForGameOfThorns ++ [ fchar ] }

            Nothing ->
                ai_helper_func currentDisplay currentFloorId fcharId opponents_and_player_rec2


attackIfClose_OtherwiseMove : Beings.FightingCharacter -> Beings.Player -> Int -> Grid.Grid Tile -> Dict Int FloorStore -> List Int -> { fightingCharacter : Beings.FightingCharacter, player : Beings.Player, mbFightingCharacterForGameOfThorns : Maybe Beings.FightingCharacter, textMsg : String, lrandInts : List Int }
attackIfClose_OtherwiseMove fightingCharacter player currentFloorId grid floorDict pseudoRandomIntsPool =
    if fightingCharacter.floorId /= currentFloorId && fightingCharacter.health > 0 then
        let
            ( updatedFightingCharacter, updatedRandInts ) =
                fightingCharacterMove fightingCharacter player currentFloorId grid floorDict pseudoRandomIntsPool
        in
        { fightingCharacter = updatedFightingCharacter, player = player, mbFightingCharacterForGameOfThorns = Nothing, textMsg = "", lrandInts = updatedRandInts }

    else
        case List.filter (\location -> location == player.location) (Grid.neighborhoodCalc 1 fightingCharacter.location) of
            location :: locs ->
                if fightingCharacter.attacksUsingGameOfThorns then
                    { fightingCharacter = fightingCharacter, player = player, mbFightingCharacterForGameOfThorns = Just fightingCharacter, textMsg = "", lrandInts = pseudoRandomIntsPool }

                else
                    let
                        attackOutput =
                            attack fightingCharacter player pseudoRandomIntsPool
                    in
                    { fightingCharacter = attackOutput.dudeA
                    , player = attackOutput.dudeB
                    , mbFightingCharacterForGameOfThorns = Nothing
                    , textMsg = attackOutput.textMsg
                    , lrandInts = attackOutput.randInts
                    }

            [] ->
                let
                    ( updatedFightingCharacter, updatedRandInts ) =
                        fightingCharacterMove fightingCharacter player currentFloorId grid floorDict pseudoRandomIntsPool
                in
                { fightingCharacter = updatedFightingCharacter, player = player, mbFightingCharacterForGameOfThorns = Nothing, textMsg = "", lrandInts = updatedRandInts }


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


fightingCharacterMove : Beings.FightingCharacter -> Beings.Player -> Int -> Grid.Grid Tile -> Dict Int FloorStore -> List Int -> ( Beings.FightingCharacter, List Int )
fightingCharacterMove fightingCharacter player currentFloorId grid floorDict lRandomInts =
    if fightingCharacter.floorId /= currentFloorId then
        --BeingsInTileGrid.characterMove_differentFloor fightingCharacter player grid floorDict lRandomInts
        BeingsInTileGrid.characterMove_RandomMove fightingCharacter player grid floorDict lRandomInts

    else
        case fightingCharacter.movingStrategy of
            Just Beings.MoveTowardsPlayer ->
                BeingsInTileGrid.characterMove_sameFloorAsPlayer_moveTowardsPlayer fightingCharacter player currentFloorId grid floorDict lRandomInts

            Just Beings.MoveRandomly ->
                BeingsInTileGrid.characterMove_RandomMove fightingCharacter player grid floorDict lRandomInts

            Just Beings.DontMove ->
                ( fightingCharacter, lRandomInts )

            Nothing ->
                ( fightingCharacter, lRandomInts )
