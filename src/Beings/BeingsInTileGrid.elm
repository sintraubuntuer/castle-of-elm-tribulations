module Beings.BeingsInTileGrid exposing
    ( characterMove_RandomMove
    , characterMove_sameFloorAsPlayer_moveTowardsPlayer
    , is2dGridTileWalkable
    , isGridTileWalkable
    , isTileWalkable
    , move
    )

import Beings.Beings as Beings
import Dict exposing (Dict)
import GameModel exposing (CurrentDisplay(..), FloorStore)
import Grid2
import Grid3 as Grid
import Tile exposing (Tile(..))


move : ( Int, Int ) -> Grid.Grid Tile -> (Grid.Coordinate -> { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int } -> Grid.Grid Tile -> Bool) -> { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int } -> { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int }
move ( x_shift, y_shift ) grid isWalkableFunc a =
    let
        location =
            Grid.Coordinate (a.location.x + x_shift) (a.location.y + y_shift) a.location.z

        initiative =
            a.initiative + 100
    in
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


characterMove_sameFloorAsPlayer_moveTowardsPlayer : { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int } -> Beings.Player -> Int -> Grid.Grid Tile -> List Int -> ( { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int }, List Int )
characterMove_sameFloorAsPlayer_moveTowardsPlayer character player currentFloorId grid lRandomInts =
    let
        player_location =
            player.location

        ( xrand, yrand, updatedRandInts ) =
            ( List.head lRandomInts |> Maybe.withDefault 0
            , List.drop 1 lRandomInts
                |> List.head
                |> Maybe.withDefault 0
            , List.drop 2 lRandomInts
            )

        x_delta_toPlayer =
            player_location.x - character.location.x

        y_delta_toPlayer =
            player_location.y - character.location.y

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

        updatedCharacter =
            move ( xscaled, yscaled ) grid isGridTileWalkable character
                |> ifOccupiedByPlayerGoBackToInitialPosition character player_location
    in
    ( updatedCharacter, updatedRandInts )


characterMove_RandomMove : { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int } -> Beings.Player -> Grid.Grid Tile -> List Int -> ( { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int }, List Int )
characterMove_RandomMove character player grid lRandomInts =
    -- for now we will just use a completely RandomMove if character is on a different floor relative to player
    let
        ( xrand, yrand, updatedRandInts ) =
            ( List.head lRandomInts |> Maybe.withDefault 0
            , List.drop 1 lRandomInts
                |> List.head
                |> Maybe.withDefault 0
            , List.drop 2 lRandomInts
            )

        xscaled =
            if xrand <= 33 then
                -1

            else if xrand > 33 && xrand <= 66 then
                0

            else
                1

        yscaled =
            if yrand <= 33 then
                -1

            else if yrand > 33 && yrand <= 66 then
                0

            else
                1

        updatedCharacter =
            move ( xscaled, yscaled ) grid isGridTileWalkable character
    in
    ( updatedCharacter, updatedRandInts )


ifOccupiedByPlayerGoBackToInitialPosition : { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int } -> Grid.Coordinate -> { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int } -> { a | location : Grid.Coordinate, direction : Beings.Direction, movingStrategy : Maybe Beings.MovingStrategy, inventory : Beings.Inventory, initiative : Int }
ifOccupiedByPlayerGoBackToInitialPosition initial_char player_location fchar =
    if fchar.location == player_location then
        initial_char

    else
        fchar


isGridTileWalkable : Grid.Coordinate -> { a | inventory : Beings.Inventory } -> Grid.Grid Tile -> Bool
isGridTileWalkable location_ being grid =
    Grid.get location_ grid
        |> Maybe.map (isTileWalkable being)
        |> Maybe.withDefault False


is2dGridTileWalkable : Grid2.Coordinate -> { a | inventory : Beings.Inventory } -> Grid2.Grid Tile -> Bool
is2dGridTileWalkable location_ being grid =
    Grid2.get location_ grid
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
            List.foldl (\it bacc -> List.member it (Dict.values being.inventory) && bacc) True doorinfo.requiresToOpen

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
