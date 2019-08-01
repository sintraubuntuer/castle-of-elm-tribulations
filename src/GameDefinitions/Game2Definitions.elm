module GameDefinitions.Game2Definitions exposing (initialStateFunc)

import Dict exposing (Dict)
import GameModel exposing (RoomRectangle, TunnelRectangle)
import Grid
import MapGen


setAllAsUnexplored : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Visibility
setAllAsUnexplored level =
    let
        grid =
            Grid.toList level
    in
    List.map (\row -> List.map (\_ -> GameModel.Unexplored) row) grid |> Grid.fromList


initialPlayer : GameModel.Player
initialPlayer =
    let
        elem =
            "@"

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    GameModel.player elem "You"


initialEnemy : GameModel.EnemyId -> GameModel.Enemy
initialEnemy enemyid =
    let
        elem =
            "e" ++ String.fromInt enemyid

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    GameModel.enemy elem enemyid ("enemy" ++ String.fromInt enemyid)


dimensions : ( Int, Int )
dimensions =
    ( get_total_width 11, get_total_height 7 )



--( 92, 60 )


initialStateFunc : ( GameModel.State, Bool )
initialStateFunc =
    let
        player =
            initialPlayer

        enemy =
            initialEnemy 1

        enemy2 =
            initialEnemy 2

        levers =
            Dict.empty

        createRandomMap =
            False
    in
    -- GameModel.State
    ( { player = player
      , enemies =
            Dict.fromList
                [ ( 1, enemy )
                , ( 2, enemy2 )
                ]
      , level = gridCaverns -- Grid.Grid Tile

      --, levers = levers --Dict LeverId LeverInfo
      , explored = setAllAsUnexplored gridCaverns -- Grid.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , y_display_anchor = 3 --Int   , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , window_width = 12
      , window_height = 12
      , total_width = get_total_width 11
      , total_height = get_total_height 7
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = dStore
      , currentFloorId = 0
      , started = True
      }
    , createRandomMap
    )


common_window_width : Int
common_window_width =
    11


common_window_height : Int
common_window_height =
    11



--dStore : Dict Int Store


dStore : Dict Int GameModel.FloorStore
dStore =
    Dict.fromList
        [ ( 0
          , { level = gridCaverns
            , explored = setAllAsUnexplored gridCaverns
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width 11 -- caverns has 11 room columns
            , total_height = get_total_height 7 -- 7 room rows
            }
          )
        , ( 1
          , { level = gridBasement
            , explored = setAllAsUnexplored gridBasement
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width 6 -- basement has 11 room columns
            , total_height = get_total_height 7 -- 7 room rows
            }
          )
        , ( 2
          , { level = gridGroundFloor
            , explored = setAllAsUnexplored gridGroundFloor
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width 7 -- groundFloor has 7room columns
            , total_height = get_total_height 9 -- 9 room rows
            }
          )
        , ( 3
          , { level = gridFirstFloor
            , explored = setAllAsUnexplored gridFirstFloor
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width 6 -- firstFloor has 6room columns
            , total_height = get_total_height 7 -- 7 room rows
            }
          )
        , ( 4
          , { level = gridTheAttic
            , explored = setAllAsUnexplored gridTheAttic
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width 4 -- theAttic has 4 room columns
            , total_height = get_total_height 4 -- 4 room rows
            }
          )
        ]


gridInitializer : Int -> Int -> Grid.Grid GameModel.Tile
gridInitializer nr_rows nr_cols =
    Grid.initialize { width = get_total_width nr_cols, height = get_total_height nr_rows } GameModel.NoTileYet


gridGroundFloor : Grid.Grid GameModel.Tile
gridGroundFloor =
    gridInitializer 9 7
        |> addGroundFloorCustomRoomsAndTunnels


gridBasement : Grid.Grid GameModel.Tile
gridBasement =
    gridInitializer 7 6
        |> addBasementCustomRoomsAndTunnels


gridFirstFloor : Grid.Grid GameModel.Tile
gridFirstFloor =
    gridInitializer 7 6
        |> addFirstFloorCustomRoomsAndTunnels


gridTheAttic : Grid.Grid GameModel.Tile
gridTheAttic =
    gridInitializer 4 4
        |> addTheAtticCustomRoomsAndTunnels


gridCaverns : Grid.Grid GameModel.Tile
gridCaverns =
    gridInitializer 7 11
        |> addCavernsCustomRoomsAndTunnels


addCavernsCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addCavernsCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc cavernsInitialRoomRectangles
        |> MapGen.listTunnelRectangleToGridFunc (cavernsInitialHorizontalTunnelRectangles ++ cavernsStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc cavernsInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (cavernsInitialHorizontalTunnelRectangles ++ cavernsStairsTunnel ++ cavernsInitialRoomRectangles ++ cavernsInitialVerticalTunnelRectangles)
        --|> make sure if cell width x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addCavernsStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


addBasementCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addBasementCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (basementInitialRoomRectangles ++ basementCustomRoomRectangles)
        |> MapGen.listTunnelRectangleToGridFunc (basementInitialHorizontalTunnelRectangles ++ basementStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc (basementInitialVerticalTunnelRectangles ++ basementCustomVerticalTunnelRectangles)
        |> MapGen.createWallBoundaries (basementInitialRoomRectangles ++ basementCustomRoomRectangles ++ basementInitialHorizontalTunnelRectangles ++ basementStairsTunnel ++ basementInitialVerticalTunnelRectangles ++ basementCustomVerticalTunnelRectangles)
        --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addBasementStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


addGroundFloorCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addGroundFloorCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (groundFloorInitialRoomRectangles ++ groundFloorCustomRoomRectangles)
        |> MapGen.listTunnelRectangleToGridFunc (groundFloorInitialHorizontalTunnelRectangles ++ groundFloorStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc groundFloorInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (groundFloorInitialRoomRectangles ++ groundFloorCustomRoomRectangles ++ groundFloorInitialHorizontalTunnelRectangles ++ groundFloorStairsTunnel ++ groundFloorInitialVerticalTunnelRectangles)
        --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addGroundFloorStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


addFirstFloorCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addFirstFloorCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (firstFloorInitialRoomRectangles ++ firstFloorCustomRoomRectangles)
        |> MapGen.listTunnelRectangleToGridFunc (firstFloorInitialHorizontalTunnelRectangles ++ firstFloorStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc firstFloorInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (firstFloorInitialRoomRectangles ++ firstFloorCustomRoomRectangles ++ firstFloorInitialHorizontalTunnelRectangles ++ firstFloorStairsTunnel ++ firstFloorInitialVerticalTunnelRectangles)
        --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addFirstFloorStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


addTheAtticCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addTheAtticCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (theAtticInitialRoomRectangles ++ theAtticCustomRoomRectangles)
        |> MapGen.listTunnelRectangleToGridFunc (theAtticInitialHorizontalTunnelRectangles ++ theAtticStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc theAtticInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (theAtticInitialRoomRectangles ++ theAtticCustomRoomRectangles ++ theAtticInitialHorizontalTunnelRectangles ++ theAtticStairsTunnel ++ theAtticInitialVerticalTunnelRectangles)
        --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addTheAtticFloorStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


groundFloorStairsTunnel : List TunnelRectangle
groundFloorStairsTunnel =
    [ getVerticalTunnel 9 2 TunnelUp Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing (Just ( -6, 0 ))
    , getVerticalTunnel 5 6 TunnelUp Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) (Just HorizontalRoom) (Just HorizontalRoom) Nothing
    , getVerticalTunnel 3 6 TunnelDown Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) (Just HorizontalRoom) Nothing Nothing
    ]


cavernsStairsTunnel : List TunnelRectangle
cavernsStairsTunnel =
    [ getHorizontalTunnel 4 8 TunnelToTheLeft (Just (vertical_wall_width + horizontal_space_between_rooms)) Nothing Nothing Nothing Nothing
    , getVerticalTunnel 7 2 TunnelDown Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing Nothing
    ]


basementStairsTunnel : List TunnelRectangle
basementStairsTunnel =
    [ getHorizontalTunnel 6 6 TunnelToTheRight (Just (vertical_wall_width + horizontal_space_between_rooms)) Nothing Nothing Nothing Nothing
    , getVerticalTunnel 6 6 TunnelDown Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing Nothing
    ]


addCavernsStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addCavernsStairs grid =
    let
        lstairs =
            [ { room_row = 4
              , room_col = 8
              , stairsId = 1
              , current_floor_id = caverns_floor_id
              , toFloorId = basement_floor_id
              , toStairsId = 2
              , shift = ( -1, 0 )
              , mbLocationShift = Nothing
              , direction = StairsToTheLeft
              }
            , { room_row = 7
              , room_col = 2
              , stairsId = 3
              , current_floor_id = caverns_floor_id
              , toFloorId = groundFloor_id
              , toStairsId = 4
              , shift = ( 0, 1 )
              , mbLocationShift = Nothing
              , direction = StairsDown
              }
            ]
    in
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction Nothing rec.shift rec.mbLocationShift gridacc) grid lstairs


caverns_floor_id : Int
caverns_floor_id =
    0


basement_floor_id : Int
basement_floor_id =
    1


groundFloor_id : Int
groundFloor_id =
    2


firstFloor_id : Int
firstFloor_id =
    3


theAttic_id : Int
theAttic_id =
    4


addBasementStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addBasementStairs grid =
    let
        lstairs =
            [ { room_row = 6
              , room_col = 6
              , stairsId = 2
              , current_floor_id = basement_floor_id
              , toFloorId = 0
              , toStairsId = 1
              , shift = ( 1, 0 )
              , mbLocationShift = Nothing
              , direction = StairsToTheRight
              , roomType = Just SquareRoom
              }
            , { room_row = 6
              , room_col = 6
              , stairsId = 5
              , current_floor_id = basement_floor_id
              , toFloorId = groundFloor_id
              , toStairsId = 6
              , shift = ( 0, 1 )
              , mbLocationShift = Nothing
              , direction = StairsDown
              , roomType = Just SquareRoom
              }
            ]
    in
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift gridacc) grid lstairs


addGroundFloorStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addGroundFloorStairs grid =
    let
        lstairs =
            [ { room_row = 9
              , room_col = 2
              , stairsId = 4
              , current_floor_id = groundFloor_id
              , toFloorId = 0
              , toStairsId = 3
              , shift = ( 0, -1 )
              , mbLocationShift = Just ( -6, 0 )
              , direction = StairsUp
              , roomType = Nothing
              }
            , { room_row = 5
              , room_col = 6
              , stairsId = 6
              , current_floor_id = groundFloor_id
              , toFloorId = basement_floor_id
              , toStairsId = 5
              , shift = ( 0, -1 )
              , mbLocationShift = Nothing
              , direction = StairsUp
              , roomType = Just HorizontalRoom
              }
            , { room_row = 3
              , room_col = 6
              , stairsId = 7
              , current_floor_id = groundFloor_id
              , toFloorId = firstFloor_id
              , toStairsId = 8
              , shift = ( 0, 1 )
              , mbLocationShift = Nothing
              , direction = StairsDown
              , roomType = Just HorizontalRoom
              }
            ]
    in
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift gridacc) grid lstairs


type StairsOrientation
    = StairsToTheLeft
    | StairsToTheRight
    | StairsUp
    | StairsDown


getStairsOnRoom : Int -> Int -> Int -> Int -> Int -> StairsOrientation -> Maybe RoomType -> ( Int, Int ) -> Maybe ( Int, Int ) -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
getStairsOnRoom row_nr col_nr stairsId toFloorId toStairsId orientation mbFromRoomType shiftOnDestinationTuple mbShiftLocationTuple grid =
    let
        tunnel_width =
            vertical_wall_width + horizontal_space_between_rooms

        tunnel_height =
            horizontal_wall_height + vertical_space_between_rooms

        ( loc_x_shift, loc_y_shift ) =
            mbShiftLocationTuple |> Maybe.withDefault ( 0, 0 )

        ( top_left_x, top_left_y ) =
            case orientation of
                StairsToTheLeft ->
                    ( square_room_top_left_x row_nr col_nr - tunnel_width + loc_x_shift + addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y row_nr col_nr + square_room_height // 2 + loc_y_shift )

                StairsToTheRight ->
                    ( square_room_top_left_x row_nr col_nr + square_room_side + tunnel_width - 1 + loc_x_shift - addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y row_nr col_nr + square_room_height // 2 + loc_y_shift )

                StairsUp ->
                    ( square_room_top_left_x row_nr col_nr + square_room_side // 2 + loc_x_shift, square_room_top_left_y row_nr col_nr - tunnel_height + loc_y_shift + addOneIfRoomTypeHorizontal mbFromRoomType )

                StairsDown ->
                    ( square_room_top_left_x row_nr col_nr + square_room_side // 2 + loc_x_shift, square_room_top_left_y row_nr col_nr + square_room_side + tunnel_height - 1 + loc_y_shift - addOneIfRoomTypeHorizontal mbFromRoomType )

        tileStairs =
            GameModel.Stairs (GameModel.StairsInfo stairsId toFloorId toStairsId shiftOnDestinationTuple False GameModel.Unexplored)
    in
    Grid.set (Grid.Coordinate top_left_x top_left_y) tileStairs grid


theAtticInitialRoomRectangles : List RoomRectangle
theAtticInitialRoomRectangles =
    [ getRoom 1 1 SquareRoom
    , getRoom 1 2 SquareRoom
    , getRoom 1 3 HorizontalRoom
    , getRoom 1 4 SquareRoom

    --
    , getRoom 2 1 SquareRoom
    , getRoom 2 2 SquareRoom
    , getRoom 2 3 SquareRoom
    , getRoom 2 4 VerticalRoom

    --
    , getRoom 3 1 VerticalRoom
    , getRoom 3 2 SquareRoom
    , getRoom 3 3 SquareRoom
    , getRoom 3 4 SquareRoom

    --
    , getRoom 4 1 SquareRoom
    , getRoom 4 2 HorizontalRoom
    , getRoom 4 3 SquareRoom
    , getRoom 4 4 SquareRoom
    ]


theAtticInitialVerticalTunnelRectangles : List TunnelRectangle
theAtticInitialVerticalTunnelRectangles =
    [ getCommonVerticalTunnel 1 1
    , getCommonVerticalTunnel 2 1
    , getCommonVerticalTunnel 3 1

    --
    , getCommonVerticalTunnel 2 1
    , getCommonVerticalTunnel 2 2

    --
    , getCommonVerticalTunnel 2 3
    , getCommonVerticalTunnel 2 4

    --
    , getCommonVerticalTunnel 1 4
    , getCommonVerticalTunnel 2 4
    , getCommonVerticalTunnel 3 4
    ]


theAtticInitialHorizontalTunnelRectangles : List TunnelRectangle
theAtticInitialHorizontalTunnelRectangles =
    [ getCommonHorizontalTunnel 1 1
    , getCommonHorizontalTunnel 1 2
    , getCommonHorizontalTunnel 1 3

    --
    , getCommonHorizontalTunnel 2 1
    , getCommonHorizontalTunnel 2 2

    --
    , getCommonHorizontalTunnel 3 2
    , getCommonHorizontalTunnel 3 3

    --
    , getCommonHorizontalTunnel 4 1
    , getCommonHorizontalTunnel 4 2
    , getCommonHorizontalTunnel 4 3
    ]


theAtticCustomRoomRectangles =
    []


theAtticStairsTunnel =
    [ getVerticalTunnel 1 4 TunnelUp Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) (Just SquareRoom) Nothing Nothing
    ]


addTheAtticFloorStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addTheAtticFloorStairs grid =
    let
        lstairs =
            [ { room_row = 1
              , room_col = 4
              , stairsId = 10
              , current_floor_id = theAttic_id
              , toFloorId = firstFloor_id
              , toStairsId = 9
              , shift = ( 0, -1 )
              , mbLocationShift = Nothing
              , direction = StairsUp
              , roomType = Just SquareRoom
              }
            ]
    in
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift gridacc) grid lstairs


firstFloorInitialRoomRectangles : List RoomRectangle
firstFloorInitialRoomRectangles =
    [ getRoom 1 1 SquareRoom
    , getRoom 1 2 SquareRoom

    --
    , getRoom 2 1 SquareRoom
    , getRoom 2 2 SquareRoom
    , getRoom 2 3 SquareRoom
    , getRoom 2 4 SquareRoom

    --
    , getRoom 3 3 VerticalRoom
    , getRoom 3 4 SquareRoom
    , getRoom 3 5 HorizontalRoom
    , getRoom 3 6 SquareRoom

    --
    , getRoom 4 3 VerticalRoom
    , getRoom 4 4 VerticalRoom
    , getRoom 4 6 VerticalRoom

    --
    , getRoom 5 3 VerticalRoom
    , getRoom 5 4 SquareRoom
    , getRoom 5 5 HorizontalRoom
    , getRoom 5 6 SquareRoom

    --
    , getRoom 6 1 SquareRoom
    , getRoom 6 2 SquareRoom
    , getRoom 6 3 SquareRoom
    , getRoom 6 4 SquareRoom

    --
    , getRoom 7 1 SquareRoom
    , getRoom 7 2 SquareRoom
    ]


firstFloorInitialVerticalTunnelRectangles : List TunnelRectangle
firstFloorInitialVerticalTunnelRectangles =
    [ getCommonVerticalTunnel 1 1
    , getCommonVerticalTunnel 6 1

    --
    , getCommonVerticalTunnel 1 2
    , getCommonVerticalTunnel 6 2

    --
    , getCommonVerticalTunnel 2 3
    , getCommonVerticalTunnel 3 3
    , getCommonVerticalTunnel 4 3
    , getCommonVerticalTunnel 5 3

    --
    , getCommonVerticalTunnel 2 4
    , getCommonVerticalTunnel 3 4
    , getCommonVerticalTunnel 4 4
    , getCommonVerticalTunnel 5 4

    --
    , getCommonVerticalTunnel 3 6
    , getCommonVerticalTunnel 4 6
    ]


firstFloorInitialHorizontalTunnelRectangles : List TunnelRectangle
firstFloorInitialHorizontalTunnelRectangles =
    [ getCommonHorizontalTunnel 1 1

    --
    , getCommonHorizontalTunnel 2 1
    , getCommonHorizontalTunnel 2 2
    , getCommonHorizontalTunnel 2 3

    --
    , getCommonHorizontalTunnel 3 4
    , getCommonHorizontalTunnel 3 5

    --
    , getCommonHorizontalTunnel 5 4
    , getCommonHorizontalTunnel 5 5

    --
    , getCommonHorizontalTunnel 6 1
    , getCommonHorizontalTunnel 6 2
    , getCommonHorizontalTunnel 6 3

    --
    , getCommonHorizontalTunnel 7 1
    ]


firstFloorCustomRoomRectangles =
    []


firstFloorStairsTunnel =
    [ getVerticalTunnel 5 5 TunnelUp Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) (Just HorizontalRoom) Nothing Nothing
    , getVerticalTunnel 3 5 TunnelDown Nothing (Just (horizontal_wall_height + vertical_space_between_rooms)) (Just HorizontalRoom) Nothing Nothing
    ]


addFirstFloorStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addFirstFloorStairs grid =
    let
        lstairs =
            [ { room_row = 5
              , room_col = 5
              , stairsId = 8
              , current_floor_id = firstFloor_id
              , toFloorId = groundFloor_id
              , toStairsId = 7
              , shift = ( 0, -1 )
              , mbLocationShift = Nothing
              , direction = StairsUp
              , roomType = Just HorizontalRoom
              }
            , { room_row = 3
              , room_col = 5
              , stairsId = 9
              , current_floor_id = firstFloor_id
              , toFloorId = theAttic_id
              , toStairsId = 10
              , shift = ( 0, 1 )
              , mbLocationShift = Nothing
              , direction = StairsDown
              , roomType = Just HorizontalRoom
              }
            ]
    in
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift gridacc) grid lstairs


groundFloorCustomRoomRectangles : List RoomRectangle
groundFloorCustomRoomRectangles =
    [ getCustomRoom 9 2 -3 0 (square_room_side + 1) square_room_side
    ]


groundFloorInitialRoomRectangles : List RoomRectangle
groundFloorInitialRoomRectangles =
    [ getRoom 1 1 HorizontalRoom
    , getRoom 1 2 SquareRoom
    , getRoom 1 3 HorizontalRoom
    , getRoom 1 4 HorizontalRoom
    , getRoom 1 5 HorizontalRoom
    , getRoom 1 6 SquareRoom

    --
    , getRoom 2 2 VerticalRoom
    , getRoom 2 6 VerticalRoom

    ---
    , getRoom 3 2 VerticalRoom
    , getRoom 3 3 SquareRoom
    , getRoom 3 4 SquareRoom
    , getRoom 3 5 SquareRoom
    , getRoom 3 6 HorizontalRoom
    , getRoom 3 7 SquareRoom

    --
    , getRoom 4 2 VerticalRoom
    , getRoom 4 5 VerticalRoom
    , getRoom 4 6 HorizontalRoom
    , getRoom 4 7 SquareRoom

    --
    , getRoom 5 2 VerticalRoom
    , getRoom 5 3 SquareRoom
    , getRoom 5 4 SquareRoom
    , getRoom 5 5 SquareRoom
    , getRoom 5 6 HorizontalRoom
    , getRoom 5 7 SquareRoom

    --
    , getRoom 6 2 VerticalRoom
    , getRoom 6 6 VerticalRoom

    --
    , getRoom 7 2 SquareRoom
    , getRoom 7 3 SquareRoom
    , getRoom 7 4 SquareRoom
    , getRoom 7 5 SquareRoom
    , getRoom 7 6 SquareRoom

    --
    , getRoom 8 2 VerticalRoom

    --, getRoom 9 2 SquareRoom
    ]


groundFloorInitialVerticalTunnelRectangles : List TunnelRectangle
groundFloorInitialVerticalTunnelRectangles =
    [ getCommonVerticalTunnel 1 2
    , getCommonVerticalTunnel 2 2
    , getCommonVerticalTunnel 3 2
    , getCommonVerticalTunnel 4 2
    , getCommonVerticalTunnel 5 2
    , getCommonVerticalTunnel 6 2
    , getCommonVerticalTunnel 7 2
    , getCommonVerticalTunnel 8 2

    --
    , getCommonVerticalTunnel 3 5
    , getCommonVerticalTunnel 4 5

    --
    , getCommonVerticalTunnel 1 6

    --, getCommonVerticalTunnel 2 6
    , getVerticalTunnel 2 6 TunnelDown Nothing Nothing (Just VerticalRoom) (Just HorizontalRoom) Nothing

    --, getCommonVerticalTunnel 5 6
    , getVerticalTunnel 5 6 TunnelDown Nothing Nothing (Just HorizontalRoom) Nothing Nothing
    , getCommonVerticalTunnel 6 6

    --
    , getCommonVerticalTunnel 3 7
    , getCommonVerticalTunnel 4 7
    ]


groundFloorInitialHorizontalTunnelRectangles : List TunnelRectangle
groundFloorInitialHorizontalTunnelRectangles =
    [ getCommonHorizontalTunnel 1 1
    , getCommonHorizontalTunnel 1 2
    , getCommonHorizontalTunnel 1 3
    , getCommonHorizontalTunnel 1 4
    , getCommonHorizontalTunnel 1 5

    --
    --  , getCommonHorizontalTunnel 3 2
    , getHorizontalTunnel 3 2 TunnelToTheRight Nothing Nothing (Just VerticalRoom) (Just SquareRoom) Nothing
    , getCommonHorizontalTunnel 3 3
    , getCommonHorizontalTunnel 3 4
    , getCommonHorizontalTunnel 3 5
    , getCommonHorizontalTunnel 3 6

    --
    --, getCommonHorizontalTunnel 4 5
    , getHorizontalTunnel 4 5 TunnelToTheRight Nothing Nothing (Just VerticalRoom) (Just HorizontalRoom) Nothing
    , getCommonHorizontalTunnel 4 6

    --
    --, getCommonHorizontalTunnel 5 2
    , getHorizontalTunnel 5 2 TunnelToTheRight Nothing Nothing (Just VerticalRoom) (Just SquareRoom) Nothing
    , getCommonHorizontalTunnel 5 3
    , getCommonHorizontalTunnel 5 4
    , getCommonHorizontalTunnel 5 5
    , getCommonHorizontalTunnel 5 6

    --
    , getCommonHorizontalTunnel 7 2
    , getCommonHorizontalTunnel 7 3
    , getCommonHorizontalTunnel 7 4
    , getCommonHorizontalTunnel 7 5

    --
    ]


cavernsInitialRoomRectangles : List RoomRectangle
cavernsInitialRoomRectangles =
    [ getRoom 1 7 SquareRoom
    , getRoom 1 8 HorizontalRoom
    , getRoom 1 9 SquareRoom
    , getRoom 1 10 HorizontalRoom
    , getRoom 1 11 SquareRoom

    --
    , getRoom 2 1 SquareRoom
    , getRoom 2 2 SquareRoom
    , getRoom 2 3 SquareRoom
    , getRoom 2 4 SquareRoom
    , getRoom 2 5 SquareRoom
    , getRoom 2 7 VerticalRoom
    , getRoom 2 9 VerticalRoom

    --
    , getRoom 3 2 VerticalRoom
    , getRoom 3 3 SquareRoom
    , getRoom 3 5 SquareRoom
    , getRoom 3 6 HorizontalRoom
    , getRoom 3 7 SquareRoom
    , getRoom 3 8 HorizontalRoom
    , getRoom 3 9 SquareRoom

    --
    , getRoom 4 2 VerticalRoom
    , getRoom 4 3 SquareRoom
    , getRoom 4 4 SquareRoom
    , getRoom 4 5 SquareRoom
    , getRoom 4 6 SquareRoom
    , getRoom 4 7 VerticalRoom
    , getRoom 4 8 SquareRoom
    , getRoom 4 9 SquareRoom
    , getRoom 4 10 HorizontalRoom
    , getRoom 4 11 SquareRoom

    --
    , getRoom 5 2 SquareRoom
    , getRoom 5 3 HorizontalRoom
    , getRoom 5 4 HorizontalRoom
    , getRoom 5 5 SquareRoom
    , getRoom 5 6 SquareRoom
    , getRoom 5 9 VerticalRoom

    --
    , getRoom 6 2 VerticalRoom
    , getRoom 6 5 SquareRoom
    , getRoom 6 6 SquareRoom
    , getRoom 6 7 SquareRoom
    , getRoom 6 8 HorizontalRoom
    , getRoom 6 9 SquareRoom
    , getRoom 6 10 SquareRoom

    --
    , getRoom 7 2 SquareRoom
    , getRoom 7 6 SquareRoom
    , getRoom 7 7 SquareRoom
    , getRoom 7 9 SquareRoom
    ]


cavernsInitialHorizontalTunnelRectangles : List TunnelRectangle
cavernsInitialHorizontalTunnelRectangles =
    [ getCommonHorizontalTunnel 1 7
    , getCommonHorizontalTunnel 1 8
    , getCommonHorizontalTunnel 1 9
    , getCommonHorizontalTunnel 1 10

    --
    , getCommonHorizontalTunnel 2 1
    , getCommonHorizontalTunnel 2 2
    , getCommonHorizontalTunnel 2 3
    , getCommonHorizontalTunnel 2 4

    --
    , getCommonHorizontalTunnel 3 5
    , getCommonHorizontalTunnel 3 6
    , getCommonHorizontalTunnel 3 7
    , getCommonHorizontalTunnel 3 8

    --
    , getCommonHorizontalTunnel 4 3
    , getCommonHorizontalTunnel 4 4
    , getCommonHorizontalTunnel 4 5
    , getCommonHorizontalTunnel 4 8
    , getCommonHorizontalTunnel 4 9
    , getCommonHorizontalTunnel 4 10

    --
    , getCommonHorizontalTunnel 5 2
    , getCommonHorizontalTunnel 5 3
    , getCommonHorizontalTunnel 5 4
    , getCommonHorizontalTunnel 5 5

    --
    , getCommonHorizontalTunnel 6 5
    , getCommonHorizontalTunnel 6 6
    , getCommonHorizontalTunnel 6 7
    , getCommonHorizontalTunnel 6 8
    , getCommonHorizontalTunnel 6 9

    --
    , getCommonHorizontalTunnel 7 6
    ]


cavernsInitialVerticalTunnelRectangles : List TunnelRectangle
cavernsInitialVerticalTunnelRectangles =
    [ getCommonVerticalTunnel 2 2
    , getCommonVerticalTunnel 3 2
    , getCommonVerticalTunnel 4 2
    , getCommonVerticalTunnel 5 2
    , getCommonVerticalTunnel 6 2

    --
    , getCommonVerticalTunnel 2 3
    , getCommonVerticalTunnel 3 3

    --
    , getCommonVerticalTunnel 2 5
    , getCommonVerticalTunnel 5 5

    --
    , getCommonVerticalTunnel 4 6
    , getCommonVerticalTunnel 5 6
    , getCommonVerticalTunnel 6 6

    --
    , getCommonVerticalTunnel 1 7
    , getCommonVerticalTunnel 2 7
    , getCommonVerticalTunnel 3 7
    , getCommonVerticalTunnel 6 7

    --
    , getCommonVerticalTunnel 1 9
    , getCommonVerticalTunnel 2 9
    , getCommonVerticalTunnel 3 9
    , getCommonVerticalTunnel 4 9
    , getCommonVerticalTunnel 5 9
    , getCommonVerticalTunnel 6 9
    ]



{- }
   getHorizontalTunnel : Int -> Int -> TunnelRectangle
   getHorizontalTunnel row_nr col_nr orientation =
       let
           ( top_left_x, top_left_y ) =
               ( square_room_top_left_x row_nr col_nr + square_room_width, square_room_top_left_y row_nr col_nr + square_room_height // 2 )

           tunnel_height =
               1

           tunnel_width =
               2 * vertical_wall_width + vertical_space_between_rooms
       in
       TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height
-}


type HorizontalTunnelOrientation
    = TunnelToTheRight
    | TunnelToTheLeft


type VerticalTunnelOrientation
    = TunnelUp
    | TunnelDown


addOneIfRoomTypeVertical : Maybe RoomType -> Int
addOneIfRoomTypeVertical mbRoomType =
    case mbRoomType of
        Just VerticalRoom ->
            1

        _ ->
            0


addOneIfRoomTypeHorizontal : Maybe RoomType -> Int
addOneIfRoomTypeHorizontal mbRoomType =
    case mbRoomType of
        Just HorizontalRoom ->
            1

        _ ->
            0


getCommonHorizontalTunnel : Int -> Int -> TunnelRectangle
getCommonHorizontalTunnel row_nr col_nr =
    getHorizontalTunnel row_nr col_nr TunnelToTheRight Nothing Nothing Nothing Nothing Nothing


getHorizontalTunnel : Int -> Int -> HorizontalTunnelOrientation -> Maybe Int -> Maybe Int -> Maybe RoomType -> Maybe RoomType -> Maybe ( Int, Int ) -> TunnelRectangle
getHorizontalTunnel row_nr col_nr orientation mbWidth mbHeight mbFromRoomType mbToRoomType mbShift =
    let
        tunnel_width =
            case mbWidth of
                Just awidth ->
                    awidth

                Nothing ->
                    (2 * vertical_wall_width + horizontal_space_between_rooms) + addOneIfRoomTypeVertical mbFromRoomType + addOneIfRoomTypeVertical mbToRoomType

        ( x_shift, y_shift ) =
            mbShift |> Maybe.withDefault ( 0, 0 )

        ( top_left_x, top_left_y ) =
            case orientation of
                TunnelToTheLeft ->
                    ( square_room_top_left_x row_nr col_nr - tunnel_width + x_shift + addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y row_nr col_nr + square_room_height // 2 + y_shift )

                TunnelToTheRight ->
                    ( square_room_top_left_x row_nr col_nr + square_room_width + x_shift - addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y row_nr col_nr + square_room_height // 2 + y_shift )

        tunnel_height =
            mbHeight |> Maybe.withDefault 1
    in
    TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height


getCommonVerticalTunnel : Int -> Int -> TunnelRectangle
getCommonVerticalTunnel row_nr col_nr =
    getVerticalTunnel row_nr col_nr TunnelDown Nothing Nothing Nothing Nothing Nothing


getVerticalTunnel : Int -> Int -> VerticalTunnelOrientation -> Maybe Int -> Maybe Int -> Maybe RoomType -> Maybe RoomType -> Maybe ( Int, Int ) -> TunnelRectangle
getVerticalTunnel row_nr col_nr orientation mbWidth mbHeight mbFromRoomType mbToRoomType mbShift =
    let
        tunnel_height =
            --mbHeight |> Maybe.withDefault (2 * horizontal_wall_height + vertical_space_between_rooms)
            case mbHeight of
                Just aheight ->
                    aheight

                Nothing ->
                    (2 * horizontal_wall_height + vertical_space_between_rooms) + addOneIfRoomTypeHorizontal mbFromRoomType + addOneIfRoomTypeHorizontal mbToRoomType

        tunnel_width =
            mbWidth |> Maybe.withDefault 1

        ( x_shift, y_shift ) =
            mbShift |> Maybe.withDefault ( 0, 0 )

        ( top_left_x, top_left_y ) =
            case orientation of
                TunnelUp ->
                    ( square_room_top_left_x row_nr col_nr + square_room_side // 2 + x_shift, square_room_top_left_y row_nr col_nr - tunnel_height + y_shift + addOneIfRoomTypeHorizontal mbFromRoomType )

                TunnelDown ->
                    ( square_room_top_left_x row_nr col_nr + square_room_side // 2 + x_shift, square_room_top_left_y row_nr col_nr + square_room_side + y_shift - addOneIfRoomTypeHorizontal mbFromRoomType )
    in
    TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height


getCustomVerticalTunnel : Int -> Int -> Int -> Int -> TunnelRectangle
getCustomVerticalTunnel row_nr col_nr x_shift tunnel_height_ =
    let
        ( top_left_x, top_left_y ) =
            ( square_room_top_left_x row_nr col_nr + square_room_width // 2 + x_shift, square_room_top_left_y row_nr col_nr + square_room_side )

        tunnel_height =
            tunnel_height_

        tunnel_width =
            1
    in
    TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height


get_total_width nr_rooms_per_row =
    border_left_width + border_right_width + nr_rooms_per_row * (square_room_width + 2 * vertical_wall_width) + (nr_rooms_per_row - 1) * horizontal_space_between_rooms


get_total_height nr_rooms_per_column =
    border_top_height + border_bottom_height + nr_rooms_per_column * (square_room_height + 2 * horizontal_wall_height) + (nr_rooms_per_column - 1) * vertical_space_between_rooms


square_room_side : Int
square_room_side =
    7


square_room_width : Int
square_room_width =
    square_room_side


square_room_height : Int
square_room_height =
    square_room_side


small_width_room_width : Int
small_width_room_width =
    square_room_side - 2


small_width_room_height : Int
small_width_room_height =
    square_room_side


small_height_room_width : Int
small_height_room_width =
    square_room_side


small_height_room_height : Int
small_height_room_height =
    square_room_side - 2


horizontal_wall_height : Int
horizontal_wall_height =
    1


vertical_wall_width : Int
vertical_wall_width =
    1


horizontal_space_between_rooms : Int
horizontal_space_between_rooms =
    1


vertical_space_between_rooms : Int
vertical_space_between_rooms =
    1


border_top_height : Int
border_top_height =
    1


border_left_width : Int
border_left_width =
    1


border_bottom_height : Int
border_bottom_height =
    4


border_right_width : Int
border_right_width =
    4


square_room_top_left_y : Int -> Int -> Int
square_room_top_left_y row_nr col_nr =
    border_top_height + (row_nr * horizontal_wall_height + (row_nr - 1) * (square_room_height + horizontal_wall_height + vertical_space_between_rooms)) + 1


square_room_top_left_x : Int -> Int -> Int
square_room_top_left_x row_nr col_nr =
    border_left_width + (col_nr * vertical_wall_width + (col_nr - 1) * (square_room_width + vertical_wall_width + horizontal_space_between_rooms)) + 1


small_width_room_top_left_y : Int -> Int -> Int
small_width_room_top_left_y row_nr col_nr =
    square_room_top_left_y row_nr col_nr


small_width_room_top_left_x : Int -> Int -> Int
small_width_room_top_left_x row_nr col_nr =
    square_room_top_left_x row_nr col_nr + 1


small_height_room_top_left_y : Int -> Int -> Int
small_height_room_top_left_y row_nr col_nr =
    square_room_top_left_y row_nr col_nr + 1


small_height_room_top_left_x : Int -> Int -> Int
small_height_room_top_left_x row_nr col_nr =
    square_room_top_left_x row_nr col_nr


type RoomType
    = SquareRoom
    | HorizontalRoom
    | VerticalRoom


getRoom : Int -> Int -> RoomType -> RoomRectangle
getRoom row_nr col_nr room_type =
    let
        ( top_left_x, top_left_y ) =
            case room_type of
                SquareRoom ->
                    ( square_room_top_left_x row_nr col_nr, square_room_top_left_y row_nr col_nr )

                HorizontalRoom ->
                    ( small_height_room_top_left_x row_nr col_nr, small_height_room_top_left_y row_nr col_nr )

                VerticalRoom ->
                    ( small_width_room_top_left_x row_nr col_nr, small_width_room_top_left_y row_nr col_nr )

        ( room_width, room_height ) =
            case room_type of
                SquareRoom ->
                    ( square_room_width, square_room_height )

                HorizontalRoom ->
                    ( small_height_room_width, small_height_room_height )

                VerticalRoom ->
                    ( small_width_room_width, small_width_room_height )
    in
    RoomRectangle top_left_x top_left_y room_width room_height


getCustomRoom : Int -> Int -> Int -> Int -> Int -> Int -> RoomRectangle
getCustomRoom row_nr col_nr shift_x shift_y rwidth rheight =
    let
        room =
            getRoom row_nr col_nr SquareRoom

        new_room =
            RoomRectangle (room.top_left_x + shift_x) (room.top_left_y + shift_y) rwidth rheight
    in
    new_room


basementInitialRoomRectangles : List RoomRectangle
basementInitialRoomRectangles =
    [ getRoom 1 2 SquareRoom
    , getRoom 1 5 SquareRoom
    , getRoom 2 1 SquareRoom
    , getRoom 2 2 SquareRoom
    , getRoom 2 3 SquareRoom
    , getRoom 2 4 SquareRoom
    , getRoom 2 5 SquareRoom
    , getRoom 3 2 SquareRoom
    , getRoom 3 3 SquareRoom
    , getRoom 3 4 SquareRoom
    , getRoom 3 5 SquareRoom

    --
    , getRoom 5 2 SquareRoom
    , getRoom 5 3 SquareRoom
    , getRoom 5 4 SquareRoom
    , getRoom 5 5 SquareRoom
    , getRoom 6 1 SquareRoom
    , getRoom 6 2 SquareRoom
    , getRoom 6 3 SquareRoom
    , getRoom 6 4 SquareRoom
    , getRoom 6 5 SquareRoom
    , getRoom 6 6 SquareRoom
    ]


basementInitialHorizontalTunnelRectangles : List TunnelRectangle
basementInitialHorizontalTunnelRectangles =
    [ getCommonHorizontalTunnel 2 1
    , getCommonHorizontalTunnel 2 2
    , getCommonHorizontalTunnel 2 3
    , getCommonHorizontalTunnel 2 4

    --
    , getCommonHorizontalTunnel 3 2
    , getCommonHorizontalTunnel 3 3
    , getCommonHorizontalTunnel 3 4

    --
    , getCommonHorizontalTunnel 5 2
    , getCommonHorizontalTunnel 5 3
    , getCommonHorizontalTunnel 5 4

    --
    , getCommonHorizontalTunnel 6 1
    , getCommonHorizontalTunnel 6 2
    , getCommonHorizontalTunnel 6 3
    , getCommonHorizontalTunnel 6 4
    , getCommonHorizontalTunnel 6 5
    ]


basementInitialVerticalTunnelRectangles : List TunnelRectangle
basementInitialVerticalTunnelRectangles =
    [ getCommonVerticalTunnel 1 2
    , getCommonVerticalTunnel 1 5

    --
    , getCommonVerticalTunnel 2 2
    , getCommonVerticalTunnel 2 3
    , getCommonVerticalTunnel 2 4
    , getCommonVerticalTunnel 2 5

    --
    , getCommonVerticalTunnel 5 2
    , getCommonVerticalTunnel 5 3
    , getCommonVerticalTunnel 5 4
    , getCommonVerticalTunnel 5 5
    ]


basementCustomVerticalTunnelRectangles =
    [ getVerticalTunnel 3 2 TunnelDown Nothing (Just (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)) Nothing Nothing (Just ( 0, 0 ))
    , getVerticalTunnel 3 5 TunnelDown Nothing (Just (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)) Nothing Nothing (Just ( 0, 0 ))
    , getVerticalTunnel 3 3 TunnelDown Nothing (Just (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)) Nothing Nothing (Just ( -2, 0 ))
    , getVerticalTunnel 3 4 TunnelDown Nothing (Just (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)) Nothing Nothing (Just ( 2, 0 ))

    --
    , getVerticalTunnel 3 3 TunnelDown Nothing (Just (2 * horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing (Just ( 2, 0 ))
    , getVerticalTunnel 3 4 TunnelDown Nothing (Just (2 * horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing (Just ( -2, 0 ))

    --
    , getVerticalTunnel 4 3 TunnelDown Nothing (Just (2 * horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing (Just ( 2, 0 ))
    , getVerticalTunnel 4 3 TunnelDown Nothing (Just (2 * horizontal_wall_height + vertical_space_between_rooms)) Nothing Nothing (Just ( 8, 0 ))
    ]


basementCustomRoomRectangles =
    [ getCustomRoom 4 3 5 0 square_room_side square_room_side
    ]
