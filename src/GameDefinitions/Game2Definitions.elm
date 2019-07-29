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
    ( 92, 60 )


initialStateFunc : GameModel.State
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

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            -- MapGen.randomCave dimensions
            Grid.initialize { width = w, height = h } GameModel.NoTileYet
                |> addCustomRoomsAndTunnels

        roomsInfo =
            GameModel.RoomsInfo [] 20 12 7

        firstExplored =
            setAllAsUnexplored firstMap
    in
    -- GameModel.State
    { player = player
    , enemies =
        Dict.fromList
            [ ( 1, enemy )
            , ( 2, enemy2 )
            ]
    , level = firstMap -- Grid.Grid Tile
    , levers = levers --Dict LeverId LeverInfo
    , explored = firstExplored -- Grid.Grid Visibility
    , log = [ "you enter the dungeon" ] --List String
    , pseudoRandomIntsPool = [] -- List Int
    , x_display_anchor = 3 -- Int
    , y_display_anchor = 3 --Int
    , window_width = 15
    , window_height = 15
    , total_width = Tuple.first dimensions
    , total_height = Tuple.second dimensions
    , wallPercentage = Nothing -- Maybe Float
    , roomsInfo = roomsInfo --  RoomsInfo
    }


addCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc cavernsInitialRoomRectangles
        |> MapGen.listTunnelRectangleToGridFunc cavernsInitialHorizontalTunnelRectangles
        |> MapGen.listTunnelRectangleToGridFunc cavernsInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (cavernsInitialHorizontalTunnelRectangles ++ cavernsInitialRoomRectangles ++ cavernsInitialVerticalTunnelRectangles)
        --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


cavernsInitialRoomRectangles =
    [ RoomRectangle 51 3 5 5
    , RoomRectangle 59 4 5 3
    , RoomRectangle 67 3 5 5
    , RoomRectangle 75 4 5 3
    , RoomRectangle 83 3 5 5
    , RoomRectangle 3 11 5 5
    , RoomRectangle 11 11 5 5
    , RoomRectangle 19 11 5 5
    , RoomRectangle 27 11 5 5
    , RoomRectangle 35 11 5 5
    , RoomRectangle 52 11 3 5
    , RoomRectangle 68 11 3 5
    , RoomRectangle 12 19 3 5
    , RoomRectangle 19 19 5 5
    , RoomRectangle 35 19 5 5
    , RoomRectangle 43 20 5 3
    , RoomRectangle 51 19 5 5
    , RoomRectangle 59 20 5 3
    , RoomRectangle 67 19 5 5
    , RoomRectangle 12 27 3 5
    , RoomRectangle 19 27 5 5
    , RoomRectangle 27 27 5 5
    , RoomRectangle 35 27 5 5
    , RoomRectangle 43 27 5 5
    , RoomRectangle 52 27 3 5
    , RoomRectangle 59 27 5 5
    , RoomRectangle 67 27 5 5
    , RoomRectangle 75 28 5 3
    , RoomRectangle 83 27 5 5
    , RoomRectangle 11 35 5 5
    , RoomRectangle 19 36 5 3
    , RoomRectangle 27 36 5 3
    , RoomRectangle 35 35 5 5
    , RoomRectangle 43 35 5 5
    , RoomRectangle 68 35 3 5
    , RoomRectangle 12 43 3 5
    , RoomRectangle 35 43 5 5
    , RoomRectangle 43 43 5 5
    , RoomRectangle 51 43 5 5
    , RoomRectangle 59 44 5 3
    , RoomRectangle 67 43 5 5
    , RoomRectangle 75 43 5 5
    , RoomRectangle 11 51 5 5
    , RoomRectangle 43 51 5 5
    , RoomRectangle 51 51 5 5
    , RoomRectangle 67 51 5 5
    ]


cavernsInitialHorizontalTunnelRectangles =
    [ TunnelRectangle 56 5 3 1
    , TunnelRectangle 64 5 3 1
    , TunnelRectangle 72 5 3 1
    , TunnelRectangle 80 5 3 1
    , TunnelRectangle 8 13 3 1
    , TunnelRectangle 16 13 3 1
    , TunnelRectangle 24 13 3 1
    , TunnelRectangle 32 13 3 1
    , TunnelRectangle 40 21 3 1
    , TunnelRectangle 48 21 3 1
    , TunnelRectangle 56 21 3 1
    , TunnelRectangle 64 21 3 1
    , TunnelRectangle 24 29 3 1
    , TunnelRectangle 32 29 3 1
    , TunnelRectangle 40 29 3 1
    , TunnelRectangle 64 29 3 1
    , TunnelRectangle 72 29 3 1
    , TunnelRectangle 80 29 3 1
    , TunnelRectangle 16 37 3 1
    , TunnelRectangle 24 37 3 1
    , TunnelRectangle 32 37 3 1
    , TunnelRectangle 40 37 3 1
    , TunnelRectangle 40 45 3 1
    , TunnelRectangle 48 45 3 1
    , TunnelRectangle 56 45 3 1
    , TunnelRectangle 64 45 3 1
    , TunnelRectangle 72 45 3 1
    , TunnelRectangle 48 53 3 1
    ]


cavernsInitialVerticalTunnelRectangles =
    [ TunnelRectangle 13 16 1 3
    , TunnelRectangle 13 24 1 3
    , TunnelRectangle 13 32 1 3
    , TunnelRectangle 13 40 1 3
    , TunnelRectangle 13 48 1 3
    , TunnelRectangle 21 16 1 3
    , TunnelRectangle 21 24 1 3
    , TunnelRectangle 37 16 1 3
    , TunnelRectangle 37 40 1 3
    , TunnelRectangle 53 8 1 3
    , TunnelRectangle 53 16 1 3
    , TunnelRectangle 53 24 1 3
    , TunnelRectangle 53 48 1 3
    , TunnelRectangle 69 8 1 3
    , TunnelRectangle 69 16 1 3
    , TunnelRectangle 69 24 1 3
    , TunnelRectangle 69 32 1 3
    , TunnelRectangle 69 40 1 3
    , TunnelRectangle 69 48 1 3
    ]


square_room_width : Int
square_room_width =
    5


square_room_height : Int
square_room_height =
    5


small_width_room_width : Int
small_width_room_width =
    3


small_width_room_height : Int
small_width_room_height =
    5


small_height_room_width : Int
small_height_room_width =
    5


small_height_room_height : Int
small_height_room_height =
    3


horizontal_wall_height =
    1


vertical_wall_width =
    1


horizontal_space_between_rooms =
    1


vertical_space_between_rooms =
    1


border_top_height =
    1


border_left_width =
    1


border_bottom_height =
    1


border_right_width =
    1


square_room_top_left_y : Int -> Int -> Int
square_room_top_left_y row_nr col_nr =
    border_top_height + (row_nr * horizontal_wall_height + (row_nr - 1) * (square_room_height + horizontal_wall_height + vertical_space_between_rooms))


square_room_top_left_x : Int -> Int -> Int
square_room_top_left_x row_nr col_nr =
    border_left_width + (col_nr * vertical_wall_width + (col_nr - 1) * (square_room_width + vertical_wall_width + horizontal_space_between_rooms))


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
