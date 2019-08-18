module GameDefinitions.Game2.LastFloor exposing (gridLastFloor)

import Dict exposing (Dict)
import GameDefinitions.Common exposing (StairsOrientation(..))
import GameDefinitions.Game2.ConfigParamsAndInfo
    exposing
        ( basement_floor_id
        , caverns_floor_id
        , config_params
        , firstFloor_id
        , groundFloor_id
        , holesDict
        , itemCreationDict
        , landingTargetsDict
        , lastFloor_id
        , teleporterInfoDict
        , theAttic_id
        )
import GameModel
    exposing
        ( StairsInfo
        , Tile(..)
        , WallInfo
        , defaultBrickWallInfo
        , defaultFloorInfo
        , defaultGrassInfo
        , defaultLeverInfo
        , defaultPineTreeInfo
        , defaultRoundTreeInfo
        , defaultWallInfo
        , defaultWallUpInfo
        , defaultWaterInfo
        , defaultWaterWallLeftInfo
        , defaultWaterWallUpInfo
        )
import Grid


walkableWaterInfo : GameModel.WaterInfo
walkableWaterInfo =
    { description = "just_water", isTransparent = False, isWalkable = True, isExplored = False, visibility = GameModel.Unexplored }


modelChangerFuncs : List (Grid.Coordinate -> GameModel.Model -> GameModel.Model)
modelChangerFuncs =
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
                    Grid.set (Grid.Coordinate 15 2) (GameModel.Floor GameModel.defaultFloorInfo) model.level
                        |> Grid.set (Grid.Coordinate 16 2) (GameModel.Water walkableWaterInfo)
                        |> Grid.set (Grid.Coordinate 17 2) (GameModel.Water walkableWaterInfo)
                        |> Grid.set (Grid.Coordinate 20 13) (GameModel.Grass defaultGrassInfo)
            }

        else
            { model
                | level =
                    Grid.set (Grid.Coordinate 15 2) (GameModel.Floor GameModel.defaultFloorInfo) model.level
                        |> Grid.set coords (Lever customLeverInfo)
            }
    ]


customLeverInfo : GameModel.LeverInfo
customLeverInfo =
    { isUp = False
    , modelChangerFuncs = modelChangerFuncs
    , isTransparent = False
    , isExplored = False
    , visibility = GameModel.Visible
    }


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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , ConverterTile (Tree defaultPineTreeInfo) (Lever customLeverInfo)
    , Grass defaultGrassInfo
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

    --, Wall defaultWallUpInfo
    --, Water defaultWaterWallLeftInfo
    --, Water defaultWaterInfo
    --, Floor defaultFloorInfo
    , Wall defaultWallUpInfo

    --, Floor defaultFloorInfo
    , Water defaultWaterWallLeftInfo

    --  , Floor defaultFloorInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Tree defaultRoundTreeInfo
    , Grass defaultGrassInfo
    , NoTileYet

    -- start 6th Row
    , NoTileYet
    , Wall (WallInfo False GameModel.Unexplored "three_way_at_left" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "three_way_at_top" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "corner_bottom_right" Nothing)
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Floor defaultFloorInfo
    , Wall (WallInfo False GameModel.Unexplored "cul_de_sac_at_left" Nothing)
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall defaultWallInfo
    , Wall (WallInfo False GameModel.Unexplored "three_way_at_right" Nothing)
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Tree defaultPineTreeInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Tree defaultRoundTreeInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
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
    , Water defaultWaterWallLeftInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , NoTileYet

    -- start 15th Row
    , NoTileYet
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterWallUpInfo
    , Water defaultWaterInfo
    , Water defaultWaterInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , Grass defaultGrassInfo
    , NoTileYet
    ]


addLastFloorStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addLastFloorStairs grid =
    let
        recstairs =
            { room_x = 20
            , room_y = 13
            , stairsId = 12
            , current_floor_id = lastFloor_id
            , toFloorId = groundFloor_id
            , toStairsId = 11
            , shift = ( -1, 0 )
            , mbLocationShift = Nothing
            , direction = StairsDown
            }

        tileStairs =
            GameModel.Stairs (GameModel.StairsInfo recstairs.stairsId recstairs.toFloorId recstairs.toStairsId recstairs.shift False GameModel.Unexplored)
    in
    Grid.set (Grid.Coordinate recstairs.room_x recstairs.room_y) tileStairs grid



{- }
   , defaultWaterWallUpInfo
   , defaultWaterWallLeftInfo
   , defaultWaterWallUpInfo
-}
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
    23


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
        |> addLastFloorStairs
