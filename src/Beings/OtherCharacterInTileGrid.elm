module Beings.OtherCharacterInTileGrid exposing
    ( OthersAndPlayerRec
    , otherCharacterMove
    , otherCharacter_AI
    )

import Beings.Beings as Beings
import Beings.BeingsInTileGrid
    exposing
        ( isGridTileWalkable
        , isTileWalkable
        , move
        )
import Dict exposing (Dict)
import GameModel exposing (CurrentDisplay(..))
import Grid
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
                            otherCharacterMove otherCharacter opponents_and_player_rec.player.location opponents_and_player_rec.grid opponents_and_player_rec.lrandInts
                                |> (\( fchar, lrand ) -> { opponents_and_player_rec | otherCharacters = Dict.update fcharId (\_ -> Just fchar) opponents_and_player_rec.otherCharacters, lrandInts = lrand })
                                |> (\x -> increseNrOfOtherCharacterMovesInCurrentTurn fcharId x)

                        else
                            opponents_and_player_rec
        in
        ai_helper_func currentDisplay currentFloorId fcharId opponents_and_player_rec2


otherCharacterMove : Beings.OtherCharacter -> Grid.Coordinate -> Grid.Grid Tile -> List Int -> ( Beings.OtherCharacter, List Int )
otherCharacterMove otherCharacter player_location grid lRandomInts =
    let
        ( xrand, yrand, updatedRandInts ) =
            ( List.head lRandomInts |> Maybe.withDefault 0
            , List.drop 1 lRandomInts
                |> List.head
                |> Maybe.withDefault 0
            , List.drop 2 lRandomInts
            )

        x_delta_toPlayer =
            player_location.x - otherCharacter.location.x

        y_delta_toPlayer =
            player_location.y - otherCharacter.location.y

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

        fCharacter_ =
            move ( xscaled, yscaled ) grid isGridTileWalkable otherCharacter
                |> (\fchar ->
                        if fchar.location == player_location then
                            otherCharacter

                        else
                            fchar
                   )
    in
    ( fCharacter_, updatedRandInts )
