module Beings.BeingsInTileGrid exposing
    ( isGridTileWalkable
    , isTileWalkable
    , move
    )

import Beings.Beings as Beings
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


inList : a -> List a -> Bool
inList a_val la =
    List.filter (\elem -> elem == a_val) la
        |> List.length
        |> (\x -> x > 0)
