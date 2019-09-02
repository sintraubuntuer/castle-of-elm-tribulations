module Beings.OtherCharacterInTileGrid exposing
    ( OthersAndPlayerRec
    , otherCharacterMove
    , otherCharacter_AI
    )

import Beings.Beings as Beings
import Beings.BeingsInTileGrid as BeingsInTileGrid
    exposing
        ( isGridTileWalkable
        , isTileWalkable
        , move
        )
import Dict exposing (Dict)
import GameModel exposing (CurrentDisplay(..))
import Grid3 as Grid
import Tile exposing (Tile(..))


type alias OthersAndPlayerRec =
    { otherCharacters : Dict Beings.OtherCharacterId Beings.OtherCharacter
    , player : Beings.Player
    , grid : Grid.Grid Tile
    , floorDict : Dict Int GameModel.FloorStore
    , textMsgs : List String
    , lrandInts : List Int
    }


increseNrOfOtherCharacterMovesInCurrentTurn : Beings.OtherCharacterId -> OthersAndPlayerRec -> OthersAndPlayerRec
increseNrOfOtherCharacterMovesInCurrentTurn fcharId othersAndPlayerRec =
    let
        updatedOtherCharacters =
            Dict.update fcharId (\mbOtherCharacter -> mbOtherCharacter |> Maybe.map (\fchar -> { fchar | nrMovesInCurrentTurn = fchar.nrMovesInCurrentTurn + 1 })) othersAndPlayerRec.otherCharacters
    in
    { othersAndPlayerRec | otherCharacters = updatedOtherCharacters }


otherCharacterExceedsNrMovesInCurrentTurn : Beings.OtherCharacterId -> OthersAndPlayerRec -> Bool
otherCharacterExceedsNrMovesInCurrentTurn fcharId othersAndPlayerRec =
    let
        mbOtherCharacter =
            Dict.get fcharId othersAndPlayerRec.otherCharacters
    in
    case mbOtherCharacter of
        Nothing ->
            True

        Just otherCharacter ->
            otherCharacter.nrMovesInCurrentTurn >= otherCharacter.maxNrCharacterMovesPerTurn


otherCharacter_AI : CurrentDisplay -> Int -> OthersAndPlayerRec -> OthersAndPlayerRec
otherCharacter_AI currentDisplay currentFloorId othersAndPlayerRec =
    let
        ocharIdOtherCharacterPairList =
            Dict.filter (\fcharId otherCharacter -> otherCharacter.health > 0 && otherCharacter.nrMovesInCurrentTurn < otherCharacter.maxNrCharacterMovesPerTurn) othersAndPlayerRec.otherCharacters
                |> Dict.toList
    in
    List.foldl (\enpair modelacc -> ai_helper_func currentDisplay currentFloorId (Tuple.first enpair) modelacc) othersAndPlayerRec ocharIdOtherCharacterPairList


ai_helper_func : CurrentDisplay -> Int -> Beings.OtherCharacterId -> OthersAndPlayerRec -> OthersAndPlayerRec
ai_helper_func currentDisplay currentFloorId fcharId opponents_and_player_rec =
    if otherCharacterExceedsNrMovesInCurrentTurn fcharId opponents_and_player_rec || currentDisplay == DisplayGameOfThorns then
        opponents_and_player_rec

    else
        let
            mbOtherCharacter =
                Dict.get fcharId opponents_and_player_rec.otherCharacters

            opponents_and_player_rec2 =
                case mbOtherCharacter of
                    Nothing ->
                        opponents_and_player_rec

                    Just otherCharacter ->
                        if otherCharacter.health > 0 && currentDisplay /= DisplayGameOfThorns then
                            otherCharacterMove otherCharacter opponents_and_player_rec.player currentFloorId opponents_and_player_rec.grid opponents_and_player_rec.lrandInts
                                |> (\( fchar, lrand ) -> { opponents_and_player_rec | otherCharacters = Dict.update fcharId (\_ -> Just fchar) opponents_and_player_rec.otherCharacters, lrandInts = lrand })
                                |> (\x -> increseNrOfOtherCharacterMovesInCurrentTurn fcharId x)

                        else
                            opponents_and_player_rec
        in
        ai_helper_func currentDisplay currentFloorId fcharId opponents_and_player_rec2


otherCharacterMove : Beings.OtherCharacter -> Beings.Player -> Int -> Grid.Grid Tile -> List Int -> ( Beings.OtherCharacter, List Int )
otherCharacterMove otherCharacter player currentFloorId grid lRandomInts =
    if otherCharacter.location.z /= currentFloorId then
        BeingsInTileGrid.characterMove_RandomMove otherCharacter player grid lRandomInts

    else
        case otherCharacter.movingStrategy of
            Just Beings.MoveTowardsPlayer ->
                BeingsInTileGrid.characterMove_sameFloorAsPlayer_moveTowardsPlayer otherCharacter player currentFloorId grid lRandomInts

            Just Beings.MoveRandomly ->
                BeingsInTileGrid.characterMove_RandomMove otherCharacter player grid lRandomInts

            Just Beings.DontMove ->
                ( otherCharacter, lRandomInts )

            Nothing ->
                ( otherCharacter, lRandomInts )
