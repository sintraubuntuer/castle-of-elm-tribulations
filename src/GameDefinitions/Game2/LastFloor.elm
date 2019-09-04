module GameDefinitions.Game2.LastFloor exposing (gridLastFloor, leverModelChangerFuncs)

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
import Grid2
import Grid3 as Grid
import Tile
    exposing
        ( StairsInfo
        , Tile(..)
        , Visibility(..)
        , WallInfo
        , defaultBrickWallInfo
        , defaultFloorInfo
        , defaultGrassInfo
        , defaultLeverInfo
        , defaultPineTreeInfo
        , defaultRoundTreeInfo
        , defaultVisibleGrassInfo
        , defaultWallInfo
        , defaultWallUpInfo
        , defaultWaterInfo
        , defaultWaterWallLeftInfo
        , defaultWaterWallUpInfo
        )


customLeverInfo : Tile.LeverInfo
customLeverInfo =
    { leverId = 1
    , isUp = False
    , isTransparent = False
    , isExplored = False
    , visibility = Visible
    }


modelChangerFuncsForLever1 : List (Grid.Coordinate -> GameModel.Model -> GameModel.Model)
modelChangerFuncsForLever1 =
    let
        nrEnlightenedOpponents model =
            Dict.values model.fightingCharacters |> List.filter (\en -> en.indexOfLight >= en.indexOfLightMax) |> List.length

        nrDeadOpponents model =
            Dict.values model.fightingCharacters |> List.filter (\en -> en.health <= 0) |> List.length

        reqsCompleted model =
            nrEnlightenedOpponents model >= 3 && nrDeadOpponents model == 0
    in
    [ \coords model ->
        if reqsCompleted model then
            { model
                | level =
                    model.level
                        |> Grid.set (Grid.Coordinate 15 2 lastFloor_id) (Tile.Floor Tile.defaultFloorInfo)
                        |> Grid.set (Grid.Coordinate 16 2 lastFloor_id) (Tile.Water Tile.walkableWaterInfo)
                        |> Grid.set (Grid.Coordinate 17 2 lastFloor_id) (Tile.Water Tile.walkableWaterInfo)
                        |> Grid.set (Grid.Coordinate 20 13 lastFloor_id) (Tile.Grass defaultVisibleGrassInfo)
            }

        else
            { model
                | level =
                    model.level
                        |> Grid.set (Grid.Coordinate 15 2 lastFloor_id) (Tile.Floor Tile.defaultFloorInfo)
                        |> Grid.set coords (Lever customLeverInfo)
            }
    ]


leverModelChangerFuncs : Dict Tile.LeverId GameModel.ModelChangerFuncs
leverModelChangerFuncs =
    Dict.fromList
        [ ( 1, GameModel.SimpleModelChanger modelChangerFuncsForLever1 )
        ]


lastFloorGridTiles : List (List Tile)
lastFloorGridTiles =
    [ [ NoTileYet
      , Wall (WallInfo False Unexplored "corner_top_left" Nothing)
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
      , Wall (WallInfo False Unexplored "corner_top_right" Nothing)
      , Water defaultWaterWallLeftInfo
      , Water defaultWaterInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , NoTileYet
      ]

    -- start 2nd Row
    , [ NoTileYet
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
      ]

    -- start 3rd Row
    , [ NoTileYet
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
      ]

    -- start 4th Row
    , [ NoTileYet
      , Wall defaultWallUpInfo
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Wall (WallInfo False Unexplored "cul_de_sac_at_top" Nothing)
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
      ]

    -- start 5th Row
    , [ NoTileYet
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
      ]

    -- start 6th Row
    , [ NoTileYet
      , Wall (WallInfo False Unexplored "three_way_at_left" Nothing)
      , Wall defaultWallInfo
      , Wall defaultWallInfo
      , Wall (WallInfo False Unexplored "three_way_at_top" Nothing)
      , Wall defaultWallInfo
      , Wall defaultWallInfo
      , Wall (WallInfo False Unexplored "corner_bottom_right" Nothing)
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Wall (WallInfo False Unexplored "cul_de_sac_at_left" Nothing)
      , Wall defaultWallInfo
      , Wall defaultWallInfo
      , Wall defaultWallInfo
      , Wall (WallInfo False Unexplored "three_way_at_right" Nothing)
      , Water defaultWaterWallLeftInfo
      , Water defaultWaterInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , NoTileYet
      ]

    -- start 7th Row
    , [ NoTileYet
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
      ]

    -- start 8th Row
    , [ NoTileYet
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
      ]

    -- start 9th Row
    , [ NoTileYet
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
      ]

    -- start 10th Row
    , [ NoTileYet
      , Wall defaultWallUpInfo
      , Floor defaultFloorInfo
      , Floor defaultFloorInfo
      , Wall (WallInfo False Unexplored "corner_bottom_left" Nothing)
      , Wall defaultWallInfo
      , Wall (WallInfo False Unexplored "cul_de_sac_at_right" Nothing)
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
      ]

    -- start 11th Row
    , [ NoTileYet
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
      ]

    -- start 12th Row
    , [ NoTileYet
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
      ]

    -- start 13th Row
    , [ NoTileYet
      , Wall (WallInfo False Unexplored "corner_bottom_left" Nothing)
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
      , Wall (WallInfo False Unexplored "corner_bottom_right" Nothing)
      , Water defaultWaterWallLeftInfo
      , Water defaultWaterInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , NoTileYet
      ]

    -- start 14th Row
    , [ NoTileYet
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
      ]

    -- start 15th Row
    , [ NoTileYet
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

    -- start 16th Row
    , [ NoTileYet
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
      , Water defaultWaterInfo
      , Water defaultWaterInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , Grass defaultGrassInfo
      , NoTileYet
      ]
    ]


addLastFloorStairs : Grid2.Grid Tile -> Grid2.Grid Tile
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
            Stairs (StairsInfo recstairs.stairsId recstairs.toFloorId recstairs.toStairsId recstairs.shift False Unexplored)
    in
    Grid2.set (Grid2.Coordinate recstairs.room_x recstairs.room_y) tileStairs grid


nr_cols =
    23


nr_rows =
    17


getTileAtCoords : Grid2.Coordinate -> Tile
getTileAtCoords coords =
    let
        mb_relevant_row =
            List.drop coords.y lastFloorGridTiles
                |> List.head
                |> Maybe.withDefault []
    in
    mb_relevant_row
        |> List.drop coords.x
        |> List.head
        |> Maybe.withDefault NoTileYet


gridLastFloor : Grid2.Grid Tile
gridLastFloor =
    let
        grid =
            Grid2.initialize { width = nr_cols, height = nr_rows } NoTileYet

        lcoords =
            Grid2.toCoordinates grid
    in
    List.foldl (\coords gridacc -> Grid2.set coords (getTileAtCoords coords) gridacc) grid lcoords
        |> addLastFloorStairs
