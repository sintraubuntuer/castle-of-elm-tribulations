module GameDefinitions.Game2.FirstFloor exposing (addFirstFloorCustomRoomsAndTunnels, gridFirstFloor)

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
        , defaultVerticalYellowDoorOptions
        , getCommonHorizontalTunnel
        , getCommonVerticalTunnel
        , getHolesByFloorId
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
        , teleporterInfoDict
        , theAttic_id
        )
import GameModel
    exposing
        ( RoomRectangle
        , TunnelRectangle
        )
import Grid
import MapGen
import Tile
    exposing
        ( RoomType(..)
        , TeleporterInfo
        , TeleporterType(..)
        , Tile(..)
        , WallInfo
        , defaultBrickWallInfo
        , defaultFloorInfo
        , defaultWallInfo
        , defaultWallUpInfo
        , defaultWaterInfo
        )


gridFirstFloor : Grid.Grid Tile
gridFirstFloor =
    GameDefinitions.Common.gridInitializer 7 6 config_params
        |> addFirstFloorCustomRoomsAndTunnels


addFirstFloorCustomRoomsAndTunnels : Grid.Grid Tile -> Grid.Grid Tile
addFirstFloorCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (firstFloorInitialRoomRectangles ++ firstFloorCustomRoomRectangles)
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc firstFloorInitialHorizontalTunnelRectanglesWithOptions
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc firstFloorStairsTunnelWithOptions
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc firstFloorInitialVerticalTunnelRectanglesWithOptions
        |> MapGen.createWallBoundaries (firstFloorInitialRoomRectangles ++ firstFloorCustomRoomRectangles ++ firstFloorInitialHorizontalTunnelRectangles ++ firstFloorStairsTunnel ++ firstFloorInitialVerticalTunnelRectangles)
        |> addFirstFloorStairs
        |> setHolesInGrid firstFloorHoles
        |> setLandingTargetsInGrid firstFloorLandingTargets
        |> setTeleportersInGrid config_params firstFloorTeleporters
        |> setItemsInGrid config_params firstFloorItems
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


firstFloorHoles : Dict HoleId Tile.HoleInfo
firstFloorHoles =
    getHolesByFloorId firstFloor_id holesDict


firstFloorLandingTargets : Dict TargetId LandingTargetInfo
firstFloorLandingTargets =
    getLandingTargetsByFloorId firstFloor_id landingTargetsDict


firstFloorTeleporters : Dict TeleporterId TeleporterInfo
firstFloorTeleporters =
    getTeleportersByFloorId firstFloor_id teleporterInfoDict



-- FIRSTFLOOR


firstFloorItems : Dict ItemId ItemCreationInfo
firstFloorItems =
    getItemsByFloorId firstFloor_id itemCreationDict


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
        |> List.map (\xfunc -> xfunc config_params)


firstFloorInitialVerticalTunnelRectangles : List TunnelRectangle
firstFloorInitialVerticalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) firstFloorInitialVerticalTunnelRectanglesWithOptions


firstFloorInitialVerticalTunnelRectanglesWithOptions : List ( TunnelRectangle, Tile.DoorWallOptions )
firstFloorInitialVerticalTunnelRectanglesWithOptions =
    [ ( getCommonVerticalTunnel 1 1, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 1, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 1 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 2, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 3, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 3, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 4 3, defaultVerticalGreenDoorOptions )
    , ( getCommonVerticalTunnel 5 3, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 4, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 4, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 4 4, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 4, defaultVerticalGreenDoorOptions )

    --
    , ( getCommonVerticalTunnel 3 6, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 4 6, defaultVerticalBlueDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


firstFloorInitialHorizontalTunnelRectangles : List TunnelRectangle
firstFloorInitialHorizontalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) firstFloorInitialHorizontalTunnelRectanglesWithOptions


firstFloorInitialHorizontalTunnelRectanglesWithOptions : List ( TunnelRectangle, Tile.DoorWallOptions )
firstFloorInitialHorizontalTunnelRectanglesWithOptions =
    [ ( getCommonHorizontalTunnel 1 1, defaultHorizontalGreenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 2 1, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 2, defaultHorizontalBlueDoorOptions )
    , ( getCommonHorizontalTunnel 2 3, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 3 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 5, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 5 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 5, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 6 1, defaultHorizontalRedDoorOptions )
    , ( getCommonHorizontalTunnel 6 2, defaultHorizontalYellowDoorOptions )
    , ( getCommonHorizontalTunnel 6 3, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 7 1, defaultHorizontalOpenDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


firstFloorCustomRoomRectangles : List RoomRectangle
firstFloorCustomRoomRectangles =
    []


firstFloorStairsTunnel : List TunnelRectangle
firstFloorStairsTunnel =
    List.map (\( tun, opt ) -> tun) firstFloorStairsTunnelWithOptions


firstFloorStairsTunnelWithOptions : List ( TunnelRectangle, Tile.DoorWallOptions )
firstFloorStairsTunnelWithOptions =
    [ ( getVerticalTunnel 5 5 TunnelUp Nothing (Just (config_params.horizontal_wall_height + 1)) (Just HorizontalRoom) Nothing Nothing config_params, defaultNoDoorOptions )
    , ( getVerticalTunnel 3 5 TunnelDown Nothing (Just (config_params.horizontal_wall_height + 1)) (Just HorizontalRoom) Nothing Nothing config_params
      , { left = Tile.NoDoorNoWall
        , top = Tile.UseDoor (Tile.defaultYellowDoorInfo Tile.DoorToDown)
        , right = Tile.NoDoorNoWall
        , bottom = Tile.NoDoorNoWall
        }
      )
    ]


addFirstFloorStairs : Grid.Grid Tile -> Grid.Grid Tile
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
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift config_params gridacc) grid lstairs
