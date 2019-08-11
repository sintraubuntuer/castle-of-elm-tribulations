module GameDefinitions.Game2.LastFloor exposing (gridLastFloor)

import GameModel
    exposing
        ( Tile(..)
        , WallInfo
        , defaultBrickWallInfo
        , defaultFloorInfo
        , defaultWallInfo
        , defaultWallUpInfo
        , defaultWaterInfo
        )
import Grid


lastFloorGridTiles : List GameModel.Tile
lastFloorGridTiles =
    [ NoTileYet
    , Wall (WallInfo False GameModel.Unexplored "corner_top_left" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "corner_top_right" Nothing)
    , NoTileYet

    -- start 2nd Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 3rd Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 4th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall (WallInfo False GameModel.Unexplored "cul_de_sac_at_top" Nothing)
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 5th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 6th Row
    , NoTileYet
    , Wall (WallInfo False GameModel.Unexplored "three_way_at_left" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "three_way_at_top" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "bottom_right_corner" Nothing)
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall (WallInfo False GameModel.Unexplored "cul_de_sac_at_left" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "three_way_at_right" Nothing)
    , NoTileYet

    -- start 7th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultWallUpInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 8th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 9th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 10th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall (WallInfo False GameModel.Unexplored "corner_bottom_left" Nothing)
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "cul_de_sac_at_right" Nothing)
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 11th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 12th Row
    , NoTileYet
    , Wall defaultWallUpInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall defaultWallUpInfo
    , NoTileYet

    -- start 13th Row
    , NoTileYet
    , Wall (WallInfo False GameModel.Unexplored "corner_bottom_left" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "corner_bottom_right" Nothing)
    , NoTileYet

    -- start 14th Row
    , NoTileYet
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , Wall defaultBrickWallInfo
    , NoTileYet

    -- start 15th Row
    , NoTileYet
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , NoTileYet
    ]



{- }
   getLocCoordinateFromNr nr =
       let
           nr_cols_ =
               17

           nr_rows_ =
               17

           row_nr =
               nr // nr_cols

           col_nr =
               Basics.remainderBy 17 nr
       in
       Grid.Coordinate col_nr row_nr
-}


nr_cols =
    17


nr_rows =
    17


getNrFromLocCoords : Grid.Coordinate -> Int
getNrFromLocCoords coords =
    coords.y * nr_cols + coords.x


getTileAtCoords : Grid.Coordinate -> GameModel.Tile
getTileAtCoords coords =
    let
        nr =
            getNrFromLocCoords coords
    in
    List.drop nr lastFloorGridTiles
        |> List.head
        |> Maybe.withDefault GameModel.NoTileYet


gridLastFloor : Grid.Grid GameModel.Tile
gridLastFloor =
    let
        grid =
            Grid.initialize { width = nr_cols, height = nr_rows } GameModel.NoTileYet

        lnrs =
            List.range 0 (nr_rows * nr_cols - 1)

        lcoords =
            --List.map (\nr -> getLocCoordinateFromNr nr) lnrs
            Grid.toCoordinates grid
    in
    List.foldl (\coords gridacc -> Grid.set coords (getTileAtCoords coords) gridacc) grid lcoords
