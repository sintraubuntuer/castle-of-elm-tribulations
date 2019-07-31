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
            , total_width = get_total_width 11 -- firstFloor has 11 room columns
            , total_height = get_total_height 7 -- 7 room rows
            }
          )
        , ( 1
          , { level = gridFirstFloor
            , explored = setAllAsUnexplored gridFirstFloor
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width 6 -- firstFloor has 11 room columns
            , total_height = get_total_height 7 -- 7 room rows
            }
          )
        ]


gridInitializer : Int -> Int -> Grid.Grid GameModel.Tile
gridInitializer nr_rows nr_cols =
    Grid.initialize { width = get_total_width nr_cols, height = get_total_height nr_rows } GameModel.NoTileYet


gridFirstFloor : Grid.Grid GameModel.Tile
gridFirstFloor =
    gridInitializer 7 6
        |> addBasementCustomRoomsAndTunnels


gridCaverns : Grid.Grid GameModel.Tile
gridCaverns =
    gridInitializer 7 11
        |> addCavernsCustomRoomsAndTunnels


addCavernsCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addCavernsCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc cavernsInitialRoomRectangles
        |> MapGen.listTunnelRectangleToGridFunc (cavernsInitialHorizontalTunnelRectangles ++ cavernsHorizontalStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc cavernsInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (cavernsInitialHorizontalTunnelRectangles ++ cavernsHorizontalStairsTunnel ++ cavernsInitialRoomRectangles ++ cavernsInitialVerticalTunnelRectangles)
        --|> make sure if cell width x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addCavernsStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


addBasementCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addBasementCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (basementInitialRoomRectangles ++ basementCustomRoomRectangles)
        |> MapGen.listTunnelRectangleToGridFunc (basementInitialHorizontalTunnelRectangles ++ basementHorizontalStairsTunnel)
        |> MapGen.listTunnelRectangleToGridFunc (basementInitialVerticalTunnelRectangles ++ basementCustomVerticalTunnelRectangles)
        |> MapGen.createWallBoundaries (basementInitialRoomRectangles ++ basementCustomRoomRectangles ++ basementInitialHorizontalTunnelRectangles ++ basementHorizontalStairsTunnel ++ basementInitialVerticalTunnelRectangles ++ basementCustomVerticalTunnelRectangles)
        --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addBasementStairs
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


cavernsHorizontalStairsTunnel : List TunnelRectangle
cavernsHorizontalStairsTunnel =
    [ getHorizontalTunnel 4 8 TunnelToTheLeft (Just (vertical_wall_width + vertical_space_between_rooms)) Nothing
    ]


basementHorizontalStairsTunnel : List TunnelRectangle
basementHorizontalStairsTunnel =
    [ getHorizontalTunnel 6 6 TunnelToTheRight (Just (vertical_wall_width + vertical_space_between_rooms)) Nothing
    ]


addCavernsStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addCavernsStairs grid =
    let
        lstairs =
            [ ( 4, 8 )
            ]
    in
    List.foldl (\( r_nr, c_nr ) gridacc -> getStairsOnRoom r_nr c_nr 1 1 2 ( -1, 0 ) StairsToTheLeft gridacc) grid lstairs


addBasementStairs : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addBasementStairs grid =
    let
        lstairs =
            [ ( 6, 6 )
            ]
    in
    List.foldl (\( r_nr, c_nr ) gridacc -> getStairsOnRoom r_nr c_nr 2 0 1 ( 1, 0 ) StairsToTheRight gridacc) grid lstairs


type StairsOrientation
    = StairsToTheLeft
    | StairsToTheRight


getStairsOnRoom : Int -> Int -> Int -> Int -> Int -> ( Int, Int ) -> StairsOrientation -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
getStairsOnRoom row_nr col_nr stairsId toFloorId toStairsId shiftTuple orientation grid =
    let
        tunnel_width =
            vertical_wall_width + vertical_space_between_rooms

        ( top_left_x, top_left_y ) =
            case orientation of
                StairsToTheLeft ->
                    ( square_room_top_left_x row_nr col_nr - tunnel_width, square_room_top_left_y row_nr col_nr + square_room_height // 2 )

                StairsToTheRight ->
                    ( square_room_top_left_x row_nr col_nr + square_room_side + tunnel_width - 1, square_room_top_left_y row_nr col_nr + square_room_height // 2 )

        tileStairs =
            GameModel.Stairs (GameModel.StairsInfo stairsId toFloorId toStairsId shiftTuple False GameModel.Unexplored)
    in
    Grid.set (Grid.Coordinate top_left_x top_left_y) tileStairs grid


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
    [ getVerticalTunnel 2 2
    , getVerticalTunnel 3 2
    , getVerticalTunnel 4 2
    , getVerticalTunnel 5 2
    , getVerticalTunnel 6 2

    --
    , getVerticalTunnel 2 3
    , getVerticalTunnel 3 3

    --
    , getVerticalTunnel 2 5
    , getVerticalTunnel 5 5

    --
    , getVerticalTunnel 4 6
    , getVerticalTunnel 5 6
    , getVerticalTunnel 6 6

    --
    , getVerticalTunnel 1 7
    , getVerticalTunnel 2 7
    , getVerticalTunnel 3 7
    , getVerticalTunnel 6 7

    --
    , getVerticalTunnel 1 9
    , getVerticalTunnel 2 9
    , getVerticalTunnel 3 9
    , getVerticalTunnel 4 9
    , getVerticalTunnel 5 9
    , getVerticalTunnel 6 9
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


type TunnelOrientation
    = TunnelToTheRight
    | TunnelToTheLeft


getCommonHorizontalTunnel : Int -> Int -> TunnelRectangle
getCommonHorizontalTunnel row_nr col_nr =
    getHorizontalTunnel row_nr col_nr TunnelToTheRight Nothing Nothing


getHorizontalTunnel : Int -> Int -> TunnelOrientation -> Maybe Int -> Maybe Int -> TunnelRectangle
getHorizontalTunnel row_nr col_nr orientation mbWidth mbHeight =
    let
        tunnel_width =
            mbWidth |> Maybe.withDefault (2 * vertical_wall_width + vertical_space_between_rooms)

        ( top_left_x, top_left_y ) =
            case orientation of
                TunnelToTheLeft ->
                    ( square_room_top_left_x row_nr col_nr - tunnel_width, square_room_top_left_y row_nr col_nr + square_room_height // 2 )

                TunnelToTheRight ->
                    ( square_room_top_left_x row_nr col_nr + square_room_width, square_room_top_left_y row_nr col_nr + square_room_height // 2 )

        tunnel_height =
            mbHeight |> Maybe.withDefault 1
    in
    TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height


getVerticalTunnel : Int -> Int -> TunnelRectangle
getVerticalTunnel row_nr col_nr =
    let
        ( top_left_x, top_left_y ) =
            ( square_room_top_left_x row_nr col_nr + square_room_width // 2, square_room_top_left_y row_nr col_nr + square_room_width )

        tunnel_height =
            2 * horizontal_wall_height + horizontal_space_between_rooms

        tunnel_width =
            1
    in
    TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height


getCustomVerticalTunnel : Int -> Int -> Int -> Int -> TunnelRectangle
getCustomVerticalTunnel row_nr col_nr x_shift theight =
    let
        ( top_left_x, top_left_y ) =
            ( square_room_top_left_x row_nr col_nr + square_room_width // 2 + x_shift, square_room_top_left_y row_nr col_nr + square_room_width )

        tunnel_height =
            theight

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
    1


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
    [ getVerticalTunnel 1 2
    , getVerticalTunnel 1 5

    --
    , getVerticalTunnel 2 2
    , getVerticalTunnel 2 3
    , getVerticalTunnel 2 4
    , getVerticalTunnel 2 5

    --
    , getVerticalTunnel 5 2
    , getVerticalTunnel 5 3
    , getVerticalTunnel 5 4
    , getVerticalTunnel 5 5
    ]


basementCustomVerticalTunnelRectangles =
    [ getCustomVerticalTunnel 3 2 0 (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)
    , getCustomVerticalTunnel 3 5 0 (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)
    , getCustomVerticalTunnel 3 3 -2 (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)
    , getCustomVerticalTunnel 3 4 2 (4 * horizontal_wall_height + 2 * vertical_space_between_rooms + square_room_height)

    --
    , getCustomVerticalTunnel 3 3 2 (2 * horizontal_wall_height + vertical_space_between_rooms)
    , getCustomVerticalTunnel 3 4 -2 (2 * horizontal_wall_height + vertical_space_between_rooms)

    --
    , getCustomVerticalTunnel 4 3 2 (2 * horizontal_wall_height + vertical_space_between_rooms)
    , getCustomVerticalTunnel 4 3 8 (2 * horizontal_wall_height + vertical_space_between_rooms)
    ]


basementCustomRoomRectangles =
    [ getCustomRoom 4 3 5 0 square_room_side square_room_side
    ]
