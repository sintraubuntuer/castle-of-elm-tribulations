module GameDefinitions.Game2.GroundFloor exposing (gridGroundFloor)

import Dict exposing (Dict)
import GameDefinitions.Common
    exposing
        ( HoleId
        , HorizontalTunnelOrientation(..)
        , ItemCreationInfo
        , ItemId
        , LandingTargetInfo
        , StairsOrientation(..)
        , TargetId
        , TeleporterId
        , VerticalTunnelOrientation(..)
        , defaultHorizontalBlueDoorOptions
        , defaultHorizontalGreenDoorOptions
        , defaultHorizontalOpenDoorOptions
        , defaultHorizontalRedDoorOptions
        , defaultHorizontalYellowDoorOptions
        , defaultNoDoorOptions
        , defaultVerticalBlueDoorOptions
        , defaultVerticalGreenDoorOptions
        , defaultVerticalOpenDoorOptions
        , defaultVerticalRedDoorOptions
        , defaultVerticalYellowDoorOptions
        , getCommonHorizontalTunnel
        , getCommonVerticalTunnel
        , getCustomRoom
        , getHolesByFloorId
        , getHorizontalTunnel
        , getItemsByFloorId
        , getLandingTargetsByFloorId
        , getRoom
        , getStairsOnRoom
        , getTeleportersByFloorId
        , getVerticalTunnel
        , setHolesInGrid
        , setItemsInGrid
        , setLandingTargetsInGrid
        , setTeleportersInGrid
        )
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
        ( RoomRectangle
        , RoomType(..)
        , TeleporterInfo
        , TeleporterType(..)
        , Tile(..)
        , TunnelRectangle
        , WallInfo
        , defaultBrickWallInfo
        , defaultFloorInfo
        , defaultWallInfo
        , defaultWallUpInfo
        , defaultWaterInfo
        )
import Grid
import Item
import MapGen


gridGroundFloor : Grid.Grid GameModel.Tile
gridGroundFloor =
    GameDefinitions.Common.gridInitializer 9 7 config_params
        |> addGroundFloorCustomRoomsAndTunnels


addGroundFloorCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addGroundFloorCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (groundFloorInitialRoomRectangles ++ groundFloorCustomRoomRectangles)
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc groundFloorInitialHorizontalTunnelRectanglesWithOptions
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc groundFloorStairsTunnelWithOptions
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc groundFloorInitialVerticalTunnelRectanglesWithOptions
        |> MapGen.createWallBoundaries (groundFloorInitialRoomRectangles ++ groundFloorCustomRoomRectangles ++ groundFloorInitialHorizontalTunnelRectangles ++ groundFloorStairsTunnel ++ groundFloorInitialVerticalTunnelRectangles)
        |> addGroundFloorStairs
        |> setHolesInGrid groundFloorHoles
        |> setLandingTargetsInGrid groundFloorLandingTargets
        |> setTeleportersInGrid config_params groundFloorTeleporters
        |> setItemsInGrid config_params groundFloorItems
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


groundFloorItems : Dict ItemId ItemCreationInfo
groundFloorItems =
    getItemsByFloorId groundFloor_id itemCreationDict


groundFloorHoles : Dict HoleId GameModel.HoleInfo
groundFloorHoles =
    getHolesByFloorId groundFloor_id holesDict


groundFloorLandingTargets : Dict TargetId LandingTargetInfo
groundFloorLandingTargets =
    getLandingTargetsByFloorId groundFloor_id landingTargetsDict


groundFloorTeleporters : Dict TeleporterId TeleporterInfo
groundFloorTeleporters =
    getTeleportersByFloorId groundFloor_id teleporterInfoDict



-- GROUNDFLOOR


groundFloorCustomRoomRectangles : List RoomRectangle
groundFloorCustomRoomRectangles =
    [ getCustomRoom 9 2 -3 0 (config_params.square_room_side + 1) config_params.square_room_side config_params
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
        |> List.map (\xfunc -> xfunc config_params)


groundFloorInitialVerticalTunnelRectangles : List TunnelRectangle
groundFloorInitialVerticalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) groundFloorInitialVerticalTunnelRectanglesWithOptions


groundFloorInitialVerticalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
groundFloorInitialVerticalTunnelRectanglesWithOptions =
    [ ( getCommonVerticalTunnel 1 2 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 2 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 2 config_params, defaultVerticalRedDoorOptions )
    , ( getCommonVerticalTunnel 4 2 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 2 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 2 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 7 2 config_params, defaultVerticalBlueDoorOptions )
    , ( getCommonVerticalTunnel 8 2 config_params, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 3 5 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 4 5 config_params, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 1 6 config_params, defaultVerticalGreenDoorOptions )

    --, getCommonVerticalTunnel 2 6
    , ( getVerticalTunnel 2 6 TunnelDown Nothing Nothing (Just VerticalRoom) (Just HorizontalRoom) Nothing config_params, defaultVerticalOpenDoorOptions )

    --, getCommonVerticalTunnel 5 6
    , ( getVerticalTunnel 5 6 TunnelDown Nothing Nothing (Just HorizontalRoom) Nothing Nothing config_params, defaultVerticalGreenDoorOptions )
    , ( getCommonVerticalTunnel 6 6 config_params, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 3 7 config_params, defaultVerticalBlueDoorOptions )
    , ( getCommonVerticalTunnel 4 7 config_params, defaultVerticalOpenDoorOptions )
    ]


groundFloorInitialHorizontalTunnelRectangles : List TunnelRectangle
groundFloorInitialHorizontalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) groundFloorInitialHorizontalTunnelRectanglesWithOptions


groundFloorInitialHorizontalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
groundFloorInitialHorizontalTunnelRectanglesWithOptions =
    [ ( getCommonHorizontalTunnel 1 1, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 5, defaultHorizontalRedDoorOptions )

    --
    --  , getCommonHorizontalTunnel 3 2
    , ( getHorizontalTunnel 3 2 TunnelToTheRight Nothing Nothing (Just VerticalRoom) (Just SquareRoom) Nothing, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 4, defaultHorizontalBlueDoorOptions )
    , ( getCommonHorizontalTunnel 3 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 6, defaultHorizontalOpenDoorOptions )

    --
    --, getCommonHorizontalTunnel 4 5
    , ( getHorizontalTunnel 4 5 TunnelToTheRight Nothing Nothing (Just VerticalRoom) (Just HorizontalRoom) Nothing, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 6, defaultHorizontalOpenDoorOptions )

    --
    --, getCommonHorizontalTunnel 5 2
    , ( getHorizontalTunnel 5 2 TunnelToTheRight Nothing Nothing (Just VerticalRoom) (Just SquareRoom) Nothing, defaultHorizontalGreenDoorOptions )
    , ( getCommonHorizontalTunnel 5 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 6, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 7 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 7 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 7 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 7 5, defaultHorizontalOpenDoorOptions )

    --
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


groundFloorStairsTunnelWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
groundFloorStairsTunnelWithOptions =
    [ ( getVerticalTunnel 9 2 TunnelUp Nothing (Just (config_params.horizontal_wall_height + 1)) Nothing Nothing (Just ( -6, 0 )), defaultNoDoorOptions )
    , ( getVerticalTunnel 5 6 TunnelUp Nothing (Just (config_params.horizontal_wall_height + 1)) (Just HorizontalRoom) (Just HorizontalRoom) Nothing, defaultNoDoorOptions )
    , ( getVerticalTunnel 3 6 TunnelDown Nothing (Just (config_params.horizontal_wall_height + 1)) (Just HorizontalRoom) Nothing Nothing, defaultNoDoorOptions )

    --
    , ( getHorizontalTunnel 4 7 TunnelToTheRight (Just (config_params.vertical_wall_width + 1)) Nothing Nothing Nothing Nothing
        --, defaultNoDoorOptions
      , { left = GameModel.UseDoor (customBlackDoorInfo GameModel.DoorToTheRight)

        --left = GameModel.UseDoor (GameModel.defaultBlueDoorInfo GameModel.DoorToTheRight)
        , top = GameModel.NoDoorNoWall
        , right = GameModel.NoDoorNoWall
        , bottom = GameModel.NoDoorNoWall
        }
      )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


customBlackDoorInfo : GameModel.DoorOrientation -> GameModel.DoorInfo
customBlackDoorInfo dorientation =
    { isOpen = False
    , color = Just "black"
    , orientation = dorientation
    , requiresToOpen =
        [ Item.Paper (Item.PaperInfo 1 "" "" "")
        , Item.Paper (Item.PaperInfo 2 "" "" "")
        , Item.Paper (Item.PaperInfo 3 "" "" "")
        ]
    , isExplored = False
    , visibility = GameModel.Unexplored
    }


groundFloorStairsTunnel : List TunnelRectangle
groundFloorStairsTunnel =
    List.map (\( tun, opt ) -> tun) groundFloorStairsTunnelWithOptions


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
            , { room_row = 4
              , room_col = 7
              , stairsId = 11
              , current_floor_id = groundFloor_id
              , toFloorId = lastFloor_id
              , toStairsId = 12
              , shift = ( 0, -1 )
              , mbLocationShift = Just ( 0, 0 )
              , direction = StairsToTheRight
              , roomType = Just SquareRoom
              }
            ]
    in
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift config_params gridacc) grid lstairs
