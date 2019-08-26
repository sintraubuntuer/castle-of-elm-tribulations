module GameDefinitions.Game2.Basement exposing (gridBasement)

import Dict exposing (Dict)
import GameDefinitions.Common
    exposing
        ( HoleId
        , HoleInfoWithLocation
        , HorizontalTunnelOrientation(..)
        , ItemCreationInfo
        , ItemId
        , LandingTargetInfo
        , StairsOrientation(..)
        , TargetId
        , TeleporterId
        , TeleporterInfoWithLocation
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
        , teleporterInfoDict
        , theAttic_id
        )
import GameModel
    exposing
        ( RoomRectangle
        , RoomType(..)
        , TunnelRectangle
        )
import Grid
import MapGen
import Tile
    exposing
        ( TeleporterInfo
        , TeleporterType(..)
        , Tile(..)
        , WallInfo
        , defaultBrickWallInfo
        , defaultFloorInfo
        , defaultWallInfo
        , defaultWallUpInfo
        , defaultWaterInfo
        )


gridBasement : Grid.Grid Tile
gridBasement =
    GameDefinitions.Common.gridInitializer 7 6 config_params
        |> addBasementCustomRoomsAndTunnels


addBasementCustomRoomsAndTunnels : Grid.Grid Tile -> Grid.Grid Tile
addBasementCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (basementInitialRoomRectangles ++ basementCustomRoomRectangles)
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc basementInitialHorizontalTunnelRectanglesWithOptions
        |> MapGen.listTunnelRectangleToGridFunc basementStairsTunnel
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc (basementInitialVerticalTunnelRectanglesWithOptions ++ basementCustomVerticalTunnelRectanglesWithOptions)
        |> MapGen.createWallBoundaries (basementInitialRoomRectangles ++ basementCustomRoomRectangles ++ basementInitialHorizontalTunnelRectangles ++ basementStairsTunnel ++ basementInitialVerticalTunnelRectangles ++ basementCustomVerticalTunnelRectangles)
        |> addBasementStairs
        |> setHolesInGrid basementHoles
        |> setLandingTargetsInGrid basementLandingTargets
        |> setTeleportersInGrid config_params basementTeleporters
        |> setItemsInGrid config_params basementItems
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


basementItems : Dict ItemId ItemCreationInfo
basementItems =
    getItemsByFloorId basement_floor_id itemCreationDict


basementHoles : Dict HoleId HoleInfoWithLocation
basementHoles =
    getHolesByFloorId basement_floor_id holesDict


basementLandingTargets : Dict TargetId LandingTargetInfo
basementLandingTargets =
    getLandingTargetsByFloorId basement_floor_id landingTargetsDict


basementTeleporters : Dict TeleporterId TeleporterInfoWithLocation
basementTeleporters =
    getTeleportersByFloorId basement_floor_id teleporterInfoDict



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
        |> List.map (\xfunc -> xfunc config_params)


basementInitialHorizontalTunnelRectangles : List TunnelRectangle
basementInitialHorizontalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) basementInitialHorizontalTunnelRectanglesWithOptions


basementInitialHorizontalTunnelRectanglesWithOptions : List ( TunnelRectangle, Tile.DoorWallOptions )
basementInitialHorizontalTunnelRectanglesWithOptions =
    [ ( getCommonHorizontalTunnel 2 1, defaultHorizontalYellowDoorOptions )
    , ( getCommonHorizontalTunnel 2 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 3, defaultHorizontalGreenDoorOptions )
    , ( getCommonHorizontalTunnel 2 4, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 3 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 3, defaultHorizontalRedDoorOptions )
    , ( getCommonHorizontalTunnel 3 4, defaultHorizontalGreenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 5 2, defaultHorizontalRedDoorOptions )
    , ( getCommonHorizontalTunnel 5 3, defaultHorizontalRedDoorOptions )
    , ( getCommonHorizontalTunnel 5 4, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 6 1, defaultHorizontalGreenDoorOptions )
    , ( getCommonHorizontalTunnel 6 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 4, defaultHorizontalYellowDoorOptions )
    , ( getCommonHorizontalTunnel 6 5, defaultHorizontalOpenDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


basementInitialVerticalTunnelRectangles : List TunnelRectangle
basementInitialVerticalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) basementInitialVerticalTunnelRectanglesWithOptions


basementInitialVerticalTunnelRectanglesWithOptions : List ( TunnelRectangle, Tile.DoorWallOptions )
basementInitialVerticalTunnelRectanglesWithOptions =
    [ ( getCommonVerticalTunnel 1 2, defaultVerticalBlueDoorOptions )
    , ( getCommonVerticalTunnel 1 5, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 3, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 4, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 5, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 5 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 3, defaultVerticalBlueDoorOptions )
    , ( getCommonVerticalTunnel 5 4, defaultVerticalBlueDoorOptions )
    , ( getCommonVerticalTunnel 5 5, defaultVerticalOpenDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


basementCustomVerticalTunnelRectangles : List TunnelRectangle
basementCustomVerticalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) basementCustomVerticalTunnelRectanglesWithOptions


basementCustomVerticalTunnelRectanglesWithOptions : List ( TunnelRectangle, Tile.DoorWallOptions )
basementCustomVerticalTunnelRectanglesWithOptions =
    [ ( getVerticalTunnel 3 2 TunnelDown Nothing (Just (4 * config_params.horizontal_wall_height + 2 * config_params.vertical_space_between_rooms + config_params.square_room_side)) Nothing Nothing (Just ( 0, 0 )), defaultVerticalBlueDoorOptions )
    , ( getVerticalTunnel 3 5 TunnelDown Nothing (Just (4 * config_params.horizontal_wall_height + 2 * config_params.vertical_space_between_rooms + config_params.square_room_side)) Nothing Nothing (Just ( 0, 0 )), defaultVerticalOpenDoorOptions )
    , ( getVerticalTunnel 3 3 TunnelDown Nothing (Just (4 * config_params.horizontal_wall_height + 2 * config_params.vertical_space_between_rooms + config_params.square_room_side)) Nothing Nothing (Just ( -2, 0 )), defaultVerticalGreenDoorOptions )
    , ( getVerticalTunnel 3 4 TunnelDown Nothing (Just (4 * config_params.horizontal_wall_height + 2 * config_params.vertical_space_between_rooms + config_params.square_room_side)) Nothing Nothing (Just ( 2, 0 )), defaultVerticalRedDoorOptions )

    --
    , ( getVerticalTunnel 3 3 TunnelDown Nothing (Just (2 * config_params.horizontal_wall_height + config_params.vertical_space_between_rooms)) Nothing Nothing (Just ( 2, 0 )), defaultVerticalGreenDoorOptions )
    , ( getVerticalTunnel 3 4 TunnelDown Nothing (Just (2 * config_params.horizontal_wall_height + config_params.vertical_space_between_rooms)) Nothing Nothing (Just ( -2, 0 )), defaultVerticalYellowDoorOptions )

    --
    , ( getVerticalTunnel 4 3 TunnelDown Nothing (Just (2 * config_params.horizontal_wall_height + config_params.vertical_space_between_rooms)) Nothing Nothing (Just ( 2, 0 )), defaultVerticalYellowDoorOptions )
    , ( getVerticalTunnel 4 3 TunnelDown Nothing (Just (2 * config_params.horizontal_wall_height + config_params.vertical_space_between_rooms)) Nothing Nothing (Just ( 8, 0 )), defaultVerticalYellowDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


basementCustomRoomRectangles : List RoomRectangle
basementCustomRoomRectangles =
    [ getCustomRoom 4 3 5 0 config_params.square_room_side config_params.square_room_side config_params
    ]


basementStairsTunnel : List TunnelRectangle
basementStairsTunnel =
    [ --getHorizontalTunnel 6 6 TunnelToTheRight (Just (vertical_wall_width + horizontal_space_between_rooms)) Nothing Nothing Nothing Nothing
      getHorizontalTunnel 6 6 TunnelToTheRight (Just (config_params.vertical_wall_width + 1)) Nothing Nothing Nothing Nothing config_params
    , getVerticalTunnel 6 6 TunnelDown Nothing (Just (config_params.horizontal_wall_height + 1)) Nothing Nothing Nothing config_params
    ]


addBasementStairs : Grid.Grid Tile -> Grid.Grid Tile
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
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift config_params gridacc) grid lstairs
