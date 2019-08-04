module GameDefinitions.Game2Definitions exposing (initialStateFunc)

import Dict exposing (Dict)
import GameModel exposing (HoleInfo, RoomRectangle, RoomType(..), TeleporterInfo, TeleporterType(..), TunnelRectangle)
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


initialStateFunc : ( GameModel.State, Bool, Bool )
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

        randomlyPositionPlayer =
            False

        createRandomMap =
            False
    in
    -- GameModel.State
    ( { player = { player | location = { x = 60, y = 36 } }
      , enemies =
            Dict.fromList
                [ ( 1, enemy )
                , ( 2, enemy2 )
                ]
      , level = gridGroundFloor -- Grid.Grid Tile

      --, levers = levers --Dict LeverId LeverInfo
      , explored = setAllAsUnexplored gridGroundFloor -- Grid.Grid Visibility
      , log = [ "you enter the dungeons Ground Floor " ] --List String
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , y_display_anchor = 3 --Int   , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , window_width = 12
      , window_height = 12
      , total_width = get_total_width 7
      , total_height = get_total_height 9
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = dStore
      , currentFloorId = 2
      , started = True
      }
    , createRandomMap
    , randomlyPositionPlayer
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
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc cavernsInitialHorizontalTunnelRectanglesWithOptions
        |> MapGen.listTunnelRectangleToGridFunc cavernsStairsTunnel
        |> MapGen.listTunnelRectangleToGridFunc cavernsInitialVerticalTunnelRectangles
        |> MapGen.createWallBoundaries (cavernsInitialHorizontalTunnelRectangles ++ cavernsStairsTunnel ++ cavernsInitialRoomRectangles ++ cavernsInitialVerticalTunnelRectangles)
        --|> make sure if cell width x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addCavernsStairs
        |> setLandingTargetsInGrid cavernsLandingTargets
        |> setTeleportersInGrid cavernsTeleporters
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
        |> setHolesInGrid basementHoles
        |> setLandingTargetsInGrid basementLandingTargets
        |> setTeleportersInGrid basementTeleporters
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
        |> setHolesInGrid groundFloorHoles
        |> setLandingTargetsInGrid groundFloorLandingTargets
        |> setTeleportersInGrid groundFloorTeleporters
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
        |> setHolesInGrid firstFloorHoles
        |> setLandingTargetsInGrid firstFloorLandingTargets
        |> setTeleportersInGrid firstFloorTeleporters
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
        |> setHolesInGrid theAtticHoles
        |> setLandingTargetsInGrid theAtticLandingTargets
        |> setTeleportersInGrid theAtticTeleporters
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners



-- CONFIG


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



-- HOLES


type alias HoleId =
    Int


groundFloorHoles : Dict HoleId GameModel.HoleInfo
groundFloorHoles =
    getHolesByFloorId groundFloor_id holesDict


basementHoles : Dict HoleId GameModel.HoleInfo
basementHoles =
    getHolesByFloorId basement_floor_id holesDict


firstFloorHoles : Dict HoleId GameModel.HoleInfo
firstFloorHoles =
    getHolesByFloorId firstFloor_id holesDict


theAtticHoles : Dict HoleId GameModel.HoleInfo
theAtticHoles =
    getHolesByFloorId theAttic_id holesDict


holesDict : Dict HoleId GameModel.HoleInfo
holesDict =
    let
        ( x1, y1 ) =
            get_room_position_nr 1 1 5 HorizontalRoom

        ( x2, y2 ) =
            get_room_position_nr 1 4 8 HorizontalRoom

        ( x3, y3 ) =
            get_room_position_nr 3 5 4 SquareRoom

        ( x4, y4 ) =
            get_room_position_nr 5 5 5 SquareRoom

        ( x5, y5 ) =
            get_room_position_nr 3 4 6 SquareRoom

        ( x6, y6 ) =
            get_room_position_nr 3 3 5 VerticalRoom

        ( x7, y7 ) =
            get_room_position_nr 5 3 2 VerticalRoom

        ( x8, y8 ) =
            get_room_position_nr 1 2 8 SquareRoom

        ( x9, y9 ) =
            get_room_position_nr 3 4 5 SquareRoom

        ( x10, y10 ) =
            get_room_position_nr 2 1 5 SquareRoom

        ( x11, y11 ) =
            get_room_position_nr 2 3 6 SquareRoom
    in
    Dict.fromList
        [ ( 1, createHoleInfo 1 groundFloor_id x1 y1 1 )
        , ( 2, createHoleInfo 2 groundFloor_id x2 y2 2 )
        , ( 3, createHoleInfo 3 groundFloor_id x3 y3 3 )
        , ( 4, createHoleInfo 4 basement_floor_id x4 y4 4 )
        , ( 5, createHoleInfo 5 firstFloor_id x5 y5 5 )
        , ( 6, createHoleInfo 6 firstFloor_id x6 y6 6 )
        , ( 7, createHoleInfo 7 firstFloor_id x7 y7 7 )
        , ( 8, createHoleInfo 8 theAttic_id x8 y8 8 )
        , ( 9, createHoleInfo 9 theAttic_id x9 y9 9 )
        , ( 10, createHoleInfo 10 theAttic_id x10 y10 10 )
        , ( 11, createHoleInfo 11 theAttic_id x11 y11 11 )
        ]


getHolesByFloorId : Int -> Dict HoleId HoleInfo -> Dict HoleId HoleInfo
getHolesByFloorId floorId dHoles =
    Dict.filter (\k v -> v.floorId == floorId) dHoles


createHoleInfo : Int -> Int -> Int -> Int -> Int -> GameModel.HoleInfo
createHoleInfo holeId floorId x y targetId =
    HoleInfo holeId floorId x y targetId False GameModel.Unexplored


getHoleCoordsAndHoleInfo : GameModel.HoleInfo -> ( ( Int, Int ), GameModel.HoleInfo )
getHoleCoordsAndHoleInfo holeinfo =
    ( ( holeinfo.x, holeinfo.y ), holeinfo )


setHolesInGrid : Dict HoleId HoleInfo -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
setHolesInGrid holesDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values holesDict_
                |> List.map getHoleCoordsAndHoleInfo

        tileHole h_info =
            GameModel.Hole h_info

        setTileHole xcoord ycoord h_info grid_ =
            Grid.set (Grid.Coordinate xcoord ycoord) (tileHole h_info) grid_
    in
    List.foldl (\( ( x_coord, y_coord ), hinfo ) gridacc -> setTileHole x_coord y_coord hinfo gridacc) grid lcoordsAndInfo



-- LANDING TATRGETS


type alias TargetId =
    Int


type alias LandingTargetInfo =
    { target_id : Int
    , floor_id : Int
    , x : Int
    , y : Int
    , mbLocationShift : Maybe ( Int, Int )
    }


landingTargetsDict : Dict TargetId LandingTargetInfo
landingTargetsDict =
    let
        ( x1, y1 ) =
            get_room_position_nr 2 1 5 SquareRoom

        ( x2, y2 ) =
            get_room_position_nr 4 3 5 SquareRoom

        ( x3, y3 ) =
            get_room_position_nr 6 5 5 SquareRoom

        ( x4, y4 ) =
            get_room_position_nr 3 5 5 SquareRoom

        ( x5, y5 ) =
            get_room_position_nr 3 5 6 SquareRoom

        ( x6, y6 ) =
            get_room_position_nr 3 4 5 SquareRoom

        ( x7, y7 ) =
            get_room_position_nr 5 4 2 SquareRoom

        ( x8, y8 ) =
            get_room_position_nr 5 3 8 VerticalRoom

        ( x9, y9 ) =
            get_room_position_nr 7 6 5 SquareRoom

        ( x10, y10 ) =
            get_room_position_nr 6 2 5 SquareRoom

        ( x11, y11 ) =
            get_room_position_nr 6 4 6 SquareRoom
    in
    Dict.fromList
        [ ( 1, LandingTargetInfo 1 caverns_floor_id x1 y1 Nothing )
        , ( 2, LandingTargetInfo 2 basement_floor_id x2 y2 (Just ( 5, 0 )) )
        , ( 3, LandingTargetInfo 3 basement_floor_id x3 y3 Nothing )
        , ( 4, LandingTargetInfo 4 caverns_floor_id x4 y4 Nothing )
        , ( 5, LandingTargetInfo 5 groundFloor_id x5 y5 Nothing )
        , ( 6, LandingTargetInfo 6 groundFloor_id x6 y6 Nothing )
        , ( 7, LandingTargetInfo 7 groundFloor_id x7 y7 Nothing )
        , ( 8, LandingTargetInfo 8 firstFloor_id x8 y8 Nothing )
        , ( 9, LandingTargetInfo 9 groundFloor_id x9 y9 Nothing )
        , ( 10, LandingTargetInfo 10 firstFloor_id x10 y10 Nothing )
        , ( 11, LandingTargetInfo 11 firstFloor_id x11 y11 Nothing )
        ]


cavernsLandingTargets : Dict TargetId LandingTargetInfo
cavernsLandingTargets =
    getLandingTargetsByFloorId caverns_floor_id landingTargetsDict


basementLandingTargets : Dict TargetId LandingTargetInfo
basementLandingTargets =
    getLandingTargetsByFloorId basement_floor_id landingTargetsDict


groundFloorLandingTargets : Dict TargetId LandingTargetInfo
groundFloorLandingTargets =
    getLandingTargetsByFloorId groundFloor_id landingTargetsDict


firstFloorLandingTargets : Dict TargetId LandingTargetInfo
firstFloorLandingTargets =
    getLandingTargetsByFloorId firstFloor_id landingTargetsDict


theAtticLandingTargets : Dict TargetId LandingTargetInfo
theAtticLandingTargets =
    getLandingTargetsByFloorId theAttic_id landingTargetsDict


getLandingTargetsByFloorId : Int -> Dict TargetId LandingTargetInfo -> Dict TargetId LandingTargetInfo
getLandingTargetsByFloorId floorId dlandingTargets =
    Dict.filter (\k v -> v.floor_id == floorId) dlandingTargets


getLandingTargetsCoordsAndTargetInfo : LandingTargetInfo -> ( ( Int, Int ), LandingTargetInfo )
getLandingTargetsCoordsAndTargetInfo targetInfo =
    case targetInfo.mbLocationShift of
        Nothing ->
            ( ( targetInfo.x, targetInfo.y ), targetInfo )

        Just ( x_shift, y_shift ) ->
            ( ( targetInfo.x + x_shift, targetInfo.y + y_shift ), targetInfo )


setLandingTargetsInGrid : Dict TargetId LandingTargetInfo -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
setLandingTargetsInGrid landingTargetsDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values landingTargetsDict_
                |> List.map getLandingTargetsCoordsAndTargetInfo

        tileLandingTarget lt_info =
            GameModel.Floor (createLandingTargetFloorInfo lt_info)

        setTileLandingTarget xcoord ycoord h_info grid_ =
            Grid.set (Grid.Coordinate xcoord ycoord) (tileLandingTarget h_info) grid_
    in
    List.foldl (\( ( x_coord, y_coord ), hinfo ) gridacc -> setTileLandingTarget x_coord y_coord hinfo gridacc) grid lcoordsAndInfo


createLandingTargetFloorInfo : LandingTargetInfo -> GameModel.FloorInfo
createLandingTargetFloorInfo ltinfo =
    GameModel.FloorInfo Nothing (Just (GameModel.LandingTargetDrawing ltinfo.target_id)) True True False GameModel.Unexplored ""



-- TELEPORTERS


createTeleporterInfo : Int -> Int -> TeleporterType -> Int -> Int -> RoomType -> String -> Int -> ( Int, Int ) -> GameModel.TeleporterInfo
createTeleporterInfo teleporterId floorId teleportertype row_nr col_nr room_type wall targetId shift =
    GameModel.TeleporterInfo teleporterId floorId teleportertype row_nr col_nr room_type wall targetId shift False GameModel.Unexplored


teleporterInfoDict : Dict TeleporterId TeleporterInfo
teleporterInfoDict =
    Dict.fromList
        [ ( 1, createTeleporterInfo 1 caverns_floor_id Barrel 2 5 SquareRoom "right" 2 ( 1, 0 ) )
        , ( 2, createTeleporterInfo 2 caverns_floor_id Barrel 1 7 SquareRoom "left" 1 ( -1, 0 ) )
        , ( 3, createTeleporterInfo 3 caverns_floor_id Barrel 3 5 SquareRoom "down" 4 ( 0, 1 ) )
        , ( 4, createTeleporterInfo 4 caverns_floor_id Barrel 5 5 SquareRoom "up" 3 ( 0, -1 ) )
        , ( 5, createTeleporterInfo 5 caverns_floor_id Barrel 1 11 SquareRoom "down" 6 ( 0, 1 ) )
        , ( 6, createTeleporterInfo 6 caverns_floor_id Barrel 4 11 SquareRoom "up" 5 ( 0, -1 ) )
        , ( 7, createTeleporterInfo 7 caverns_floor_id BookCase 4 7 VerticalRoom "down" 8 ( 0, 1 ) )
        , ( 8, createTeleporterInfo 8 caverns_floor_id BookCase 6 7 SquareRoom "up" 7 ( 0, -1 ) )
        , ( 9, createTeleporterInfo 9 caverns_floor_id BookCase 7 7 SquareRoom "right" 10 ( 0, -1 ) )
        , ( 10, createTeleporterInfo 10 caverns_floor_id BookCase 6 10 SquareRoom "down" 9 ( -1, 0 ) )
        , ( 11, createTeleporterInfo 11 caverns_floor_id Clock 7 2 SquareRoom "right" 12 ( 1, 0 ) )
        , ( 12, createTeleporterInfo 12 caverns_floor_id Clock 7 9 SquareRoom "left" 11 ( -1, 0 ) )
        , ( 13, createTeleporterInfo 13 basement_floor_id BookCase 2 1 SquareRoom "down" 14 ( 0, 1 ) )
        , ( 14, createTeleporterInfo 14 basement_floor_id BookCase 6 1 SquareRoom "up" 13 ( 0, -1 ) )
        , ( 15, createTeleporterInfo 15 basement_floor_id Clock 1 2 SquareRoom "right" 16 ( 1, 0 ) )
        , ( 16, createTeleporterInfo 16 basement_floor_id Clock 1 5 SquareRoom "left" 15 ( -1, 0 ) )
        , ( 17, createTeleporterInfo 17 groundFloor_id Barrel 3 3 SquareRoom "down" 18 ( 0, 1 ) )
        , ( 18, createTeleporterInfo 18 groundFloor_id Barrel 5 3 SquareRoom "up" 17 ( 0, -1 ) )
        , ( 19, createTeleporterInfo 19 groundFloor_id Barrel 6 6 VerticalRoom "left" 20 ( -1, 0 ) )
        , ( 20, createTeleporterInfo 20 firstFloor_id Barrel 6 4 SquareRoom "right" 19 ( 1, 0 ) )
        , ( 21, createTeleporterInfo 21 groundFloor_id BookCase 1 5 HorizontalRoom "down" 22 ( 0, 1 ) )
        , ( 22, createTeleporterInfo 22 groundFloor_id BookCase 7 5 HorizontalRoom "up" 21 ( 0, -1 ) )
        , ( 23, createTeleporterInfo 23 groundFloor_id BookCase 3 4 SquareRoom "down" 24 ( 0, 1 ) )
        , ( 24, createTeleporterInfo 24 groundFloor_id BookCase 5 4 SquareRoom "up" 23 ( 0, -1 ) )
        , ( 25, createTeleporterInfo 25 groundFloor_id Clock 1 2 SquareRoom "down" 26 ( 0, 1 ) )
        , ( 26, createTeleporterInfo 26 groundFloor_id Clock 7 2 SquareRoom "up" 25 ( 0, -1 ) )
        , ( 27, createTeleporterInfo 27 groundFloor_id Clock 1 6 SquareRoom "down" 28 ( 0, 1 ) )
        , ( 28, createTeleporterInfo 28 groundFloor_id Clock 7 6 SquareRoom "up" 27 ( 0, -1 ) )
        , ( 29, createTeleporterInfo 29 firstFloor_id Clock 5 3 VerticalRoom "right" 30 ( 1, 0 ) )
        , ( 30, createTeleporterInfo 30 firstFloor_id Clock 4 4 VerticalRoom "left" 29 ( -1, 0 ) )
        , ( 31, createTeleporterInfo 31 theAttic_id Clock 1 1 SquareRoom "down" 32 ( 0, 1 ) )
        , ( 32, createTeleporterInfo 32 theAttic_id Clock 2 1 SquareRoom "up" 31 ( 0, -1 ) )
        ]


type alias TeleporterId =
    Int


cavernsTeleporters : Dict TeleporterId TeleporterInfo
cavernsTeleporters =
    getTeleportersByFloorId caverns_floor_id teleporterInfoDict


basementTeleporters : Dict TeleporterId TeleporterInfo
basementTeleporters =
    getTeleportersByFloorId basement_floor_id teleporterInfoDict


groundFloorTeleporters : Dict TeleporterId TeleporterInfo
groundFloorTeleporters =
    getTeleportersByFloorId groundFloor_id teleporterInfoDict


firstFloorTeleporters : Dict TeleporterId TeleporterInfo
firstFloorTeleporters =
    getTeleportersByFloorId firstFloor_id teleporterInfoDict


theAtticTeleporters : Dict TeleporterId TeleporterInfo
theAtticTeleporters =
    getTeleportersByFloorId theAttic_id teleporterInfoDict


getTeleportersByFloorId : Int -> Dict TeleporterId TeleporterInfo -> Dict TeleporterId TeleporterInfo
getTeleportersByFloorId floorId dTeleporters =
    Dict.filter (\k v -> v.floor_id == floorId) dTeleporters


getTeleportersCoordsAndTeleportersInfo : TeleporterInfo -> ( ( Int, Int ), TeleporterInfo )
getTeleportersCoordsAndTeleportersInfo teleporterInfo =
    let
        ( x_coord, y_coord ) =
            get_teleporter_coords_from_room_row_col teleporterInfo.room_row_nr teleporterInfo.room_col_nr teleporterInfo.room_type teleporterInfo.position_in_room teleporterInfo.teleporterType
    in
    ( ( x_coord, y_coord ), teleporterInfo )


get_teleporter_coords_from_room_row_col : Int -> Int -> RoomType -> String -> TeleporterType -> ( Int, Int )
get_teleporter_coords_from_room_row_col row_nr col_nr rtype pos_in_room teleporterType =
    let
        top_left_x =
            get_room_top_left_x row_nr col_nr rtype

        top_left_y =
            get_room_top_left_y row_nr col_nr rtype

        room_width =
            get_room_width rtype

        room_height =
            get_room_height rtype

        coords =
            if String.toLower pos_in_room == "up" then
                case teleporterType of
                    Clock ->
                        ( top_left_x + room_width // 2 - 2, top_left_y - 1 )

                    _ ->
                        ( top_left_x + room_width // 2, top_left_y - 1 )

            else if String.toLower pos_in_room == "down" then
                case teleporterType of
                    Clock ->
                        ( top_left_x + room_width // 2 - 2, top_left_y + room_height )

                    _ ->
                        ( top_left_x + room_width // 2, top_left_y + room_height )

            else if String.toLower pos_in_room == "left" then
                ( top_left_x - 1, top_left_y + room_height // 2 )

            else
                -- String.toLower pos_in_room == "right" then
                ( top_left_x + room_width, top_left_y + room_height // 2 )

        _ =
            if row_nr == 7 && col_nr == 5 && rtype == HorizontalRoom then
                Debug.log "getting teleporter coords for row 7 col 5 . coords are " coords

            else
                ( -1, -1 )
    in
    coords


setTeleportersInGrid : Dict TeleporterId TeleporterInfo -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
setTeleportersInGrid teleporterDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values teleporterDict_
                |> List.map getTeleportersCoordsAndTeleportersInfo

        tryTileTeleporter xcoord ycoord tel_info grid_ =
            let
                c_wall_info =
                    Grid.get (Grid.Coordinate xcoord ycoord) grid_
            in
            case c_wall_info of
                Just (GameModel.Wall winfo) ->
                    Just (GameModel.Wall (createTeleporterWallInfo winfo tel_info))

                _ ->
                    Nothing

        setTileTeleporter xcoord ycoord t_info grid_ =
            case tryTileTeleporter xcoord ycoord t_info grid_ of
                Just atile ->
                    Grid.set (Grid.Coordinate xcoord ycoord) atile grid_

                Nothing ->
                    grid_
    in
    List.foldl (\( ( x_coord, y_coord ), tinfo ) gridacc -> setTileTeleporter x_coord y_coord tinfo gridacc) grid lcoordsAndInfo


createTeleporterWallInfo : GameModel.WallInfo -> TeleporterInfo -> GameModel.WallInfo
createTeleporterWallInfo winfo tel_info =
    { winfo | mbTeleporterObject = Just tel_info }



--GameModel.WallInfo winfo.isExplored  winfo.visibility winfo.orientation ltinfo.target_id
-- THE ATTIC


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
    [ getVerticalTunnel 1 4 TunnelUp Nothing (Just (horizontal_wall_height + 1)) (Just SquareRoom) Nothing Nothing
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



-- FIRSTFLOOR


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
    [ getVerticalTunnel 5 5 TunnelUp Nothing (Just (horizontal_wall_height + 1)) (Just HorizontalRoom) Nothing Nothing
    , getVerticalTunnel 3 5 TunnelDown Nothing (Just (horizontal_wall_height + 1)) (Just HorizontalRoom) Nothing Nothing
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



-- GROUNDFLOOR


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
    , getRoom 7 3 HorizontalRoom
    , getRoom 7 4 HorizontalRoom
    , getRoom 7 5 HorizontalRoom
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


groundFloorStairsTunnel : List TunnelRectangle
groundFloorStairsTunnel =
    [ getVerticalTunnel 9 2 TunnelUp Nothing (Just (horizontal_wall_height + 1)) Nothing Nothing (Just ( -6, 0 ))
    , getVerticalTunnel 5 6 TunnelUp Nothing (Just (horizontal_wall_height + 1)) (Just HorizontalRoom) (Just HorizontalRoom) Nothing
    , getVerticalTunnel 3 6 TunnelDown Nothing (Just (horizontal_wall_height + 1)) (Just HorizontalRoom) Nothing Nothing
    ]


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



-- BASEMENT


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


basementStairsTunnel : List TunnelRectangle
basementStairsTunnel =
    [ --getHorizontalTunnel 6 6 TunnelToTheRight (Just (vertical_wall_width + horizontal_space_between_rooms)) Nothing Nothing Nothing Nothing
      getHorizontalTunnel 6 6 TunnelToTheRight (Just (vertical_wall_width + 1)) Nothing Nothing Nothing Nothing
    , getVerticalTunnel 6 6 TunnelDown Nothing (Just (horizontal_wall_height + 1)) Nothing Nothing Nothing
    ]


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



--    CAVERNS


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


defaultHorizontalOpenDoorOptions =
    { left = GameModel.UseDoor (GameModel.defaultOpenDoorInfo GameModel.DoorToTheRight)
    , top = GameModel.NoDoorNoWall
    , right = GameModel.UseDoor (GameModel.defaultOpenDoorInfo GameModel.DoorToTheLeft)
    , bottom = GameModel.NoDoorNoWall
    }


cavernsInitialHorizontalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
cavernsInitialHorizontalTunnelRectanglesWithOptions =
    [ ( getCommonHorizontalTunnel 1 7, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 8, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 9, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 10, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 2 1, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 4, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 3 6, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 7, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 8, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 4 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 8, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 9, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 10, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 5 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 5, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 6 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 6, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 7, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 8, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 9, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 7 6, defaultHorizontalOpenDoorOptions )
    ]


cavernsInitialHorizontalTunnelRectangles : List TunnelRectangle
cavernsInitialHorizontalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) cavernsInitialHorizontalTunnelRectanglesWithOptions


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


cavernsStairsTunnel : List TunnelRectangle
cavernsStairsTunnel =
    [ --getHorizontalTunnel 4 8 TunnelToTheLeft (Just (vertical_wall_width + horizontal_space_between_rooms)) Nothing Nothing Nothing Nothing
      getHorizontalTunnel 4 8 TunnelToTheLeft (Just (vertical_wall_width + 1)) Nothing Nothing Nothing Nothing
    , getVerticalTunnel 7 2 TunnelDown Nothing (Just (horizontal_wall_height + 1)) Nothing Nothing Nothing
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



-- TYPES AND FUNCS


type StairsOrientation
    = StairsToTheLeft
    | StairsToTheRight
    | StairsUp
    | StairsDown


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



{- }
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
-}


get_total_width nr_rooms_per_row =
    border_left_width + border_right_width + nr_rooms_per_row * (square_room_width + 2 * vertical_wall_width) + (nr_rooms_per_row - 1) * horizontal_space_between_rooms


get_total_height nr_rooms_per_column =
    border_top_height + border_bottom_height + nr_rooms_per_column * (square_room_height + 2 * horizontal_wall_height) + (nr_rooms_per_column - 1) * vertical_space_between_rooms


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


getStairsOnRoom : Int -> Int -> Int -> Int -> Int -> StairsOrientation -> Maybe RoomType -> ( Int, Int ) -> Maybe ( Int, Int ) -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
getStairsOnRoom row_nr col_nr stairsId toFloorId toStairsId orientation mbFromRoomType shiftOnDestinationTuple mbShiftLocationTuple grid =
    let
        tunnel_width =
            --vertical_wall_width + horizontal_space_between_rooms
            vertical_wall_width + 1

        tunnel_height =
            --horizontal_wall_height + vertical_space_between_rooms
            horizontal_wall_height + 1

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


get_room_width : RoomType -> Int
get_room_width roomType =
    case roomType of
        SquareRoom ->
            square_room_side

        HorizontalRoom ->
            small_height_room_width

        VerticalRoom ->
            small_width_room_width


get_room_height : RoomType -> Int
get_room_height roomType =
    case roomType of
        SquareRoom ->
            square_room_height

        HorizontalRoom ->
            small_height_room_height

        VerticalRoom ->
            small_width_room_height


get_room_top_left_x : Int -> Int -> RoomType -> Int
get_room_top_left_x row_nr col_nr roomType =
    case roomType of
        SquareRoom ->
            square_room_top_left_x row_nr col_nr

        HorizontalRoom ->
            small_height_room_top_left_x row_nr col_nr

        VerticalRoom ->
            small_width_room_top_left_x row_nr col_nr


get_room_top_left_y : Int -> Int -> RoomType -> Int
get_room_top_left_y row_nr col_nr roomType =
    case roomType of
        SquareRoom ->
            square_room_top_left_y row_nr col_nr

        HorizontalRoom ->
            small_height_room_top_left_y row_nr col_nr

        VerticalRoom ->
            small_width_room_top_left_y row_nr col_nr



-- this ( room positions ) will probably have to be revised , and maybe consider roomsize in some other way ...


get_width_height_factors : Int -> ( Float, Float )
get_width_height_factors position_nr =
    if position_nr == 1 then
        ( 1.0, 1.0 )

    else if position_nr == 2 then
        ( 2.0, 1.0 )

    else if position_nr == 3 then
        ( 3.0, 1.0 )

    else if position_nr == 4 then
        ( 1.0, 2.0 )

    else if position_nr == 5 then
        ( 2.0, 2.0 )

    else if position_nr == 6 then
        ( 3.0, 2.0 )

    else if position_nr == 7 then
        ( 1.0, 3.0 )

    else if position_nr == 8 then
        ( 2.0, 3.0 )

    else
        ( 3.0, 3.0 )


get_room_position_1 : Int -> Int -> RoomType -> ( Int, Int )
get_room_position_1 row_nr col_nr roomType =
    let
        width1 =
            toFloat (get_room_width roomType - 2) / 3.0

        height1 =
            toFloat (get_room_height roomType - 2) / 3.0

        pos_x =
            toFloat (get_room_top_left_x row_nr col_nr roomType) + width1 |> Basics.round

        pos_y =
            toFloat (get_room_top_left_y row_nr col_nr roomType) + height1 |> Basics.round
    in
    ( pos_x, pos_y )


get_room_position_nr : Int -> Int -> Int -> RoomType -> ( Int, Int )
get_room_position_nr row_nr col_nr pos_nr roomType =
    let
        ( w_factor, h_factor ) =
            get_width_height_factors pos_nr

        add_width =
            toFloat (get_room_width roomType - 2) * w_factor / 3.0

        add_height =
            toFloat (get_room_height roomType - 2) * h_factor / 3.0

        pos_x =
            toFloat (get_room_top_left_x row_nr col_nr roomType) + add_width |> Basics.round

        pos_y =
            toFloat (get_room_top_left_y row_nr col_nr roomType) + add_height |> Basics.round
    in
    ( pos_x, pos_y )
