module GameDefinitions.Game2.TheAttic exposing (addTheAtticCustomRoomsAndTunnels, gridTheAttic)

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
import MapGen


gridTheAttic : Grid.Grid GameModel.Tile
gridTheAttic =
    GameDefinitions.Common.gridInitializer 4 4 config_params
        |> addTheAtticCustomRoomsAndTunnels


addTheAtticCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addTheAtticCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc (theAtticInitialRoomRectangles ++ theAtticCustomRoomRectangles)
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc theAtticInitialHorizontalTunnelRectanglesWithOptions
        |> MapGen.listTunnelRectangleToGridFunc theAtticStairsTunnel
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc theAtticInitialVerticalTunnelRectanglesWithOptions
        |> MapGen.createWallBoundaries (theAtticInitialRoomRectangles ++ theAtticCustomRoomRectangles ++ theAtticInitialHorizontalTunnelRectangles ++ theAtticStairsTunnel ++ theAtticInitialVerticalTunnelRectangles)
        |> addTheAtticFloorStairs
        |> setHolesInGrid theAtticHoles
        |> setLandingTargetsInGrid theAtticLandingTargets
        |> setTeleportersInGrid config_params theAtticTeleporters
        |> setItemsInGrid config_params theAtticItems
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners


theAtticItems : Dict ItemId ItemCreationInfo
theAtticItems =
    getItemsByFloorId theAttic_id itemCreationDict


theAtticHoles : Dict HoleId GameModel.HoleInfo
theAtticHoles =
    getHolesByFloorId theAttic_id holesDict


theAtticLandingTargets : Dict TargetId LandingTargetInfo
theAtticLandingTargets =
    getLandingTargetsByFloorId theAttic_id landingTargetsDict


theAtticTeleporters : Dict TeleporterId TeleporterInfo
theAtticTeleporters =
    getTeleportersByFloorId theAttic_id teleporterInfoDict



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
        |> List.map (\xfunc -> xfunc config_params)


theAtticInitialVerticalTunnelRectangles : List TunnelRectangle
theAtticInitialVerticalTunnelRectangles =
    List.map (\( tunnel, opts ) -> tunnel) theAtticInitialVerticalTunnelRectanglesWithOptions


theAtticInitialVerticalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
theAtticInitialVerticalTunnelRectanglesWithOptions =
    [ ( getCommonVerticalTunnel 1 1 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 1 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 1 config_params, defaultVerticalYellowDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 2 config_params, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 3 config_params, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 1 4 config_params, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 4 config_params, defaultVerticalBlueDoorOptions )
    , ( getCommonVerticalTunnel 3 4 config_params, defaultVerticalOpenDoorOptions )
    ]


theAtticInitialHorizontalTunnelRectangles : List TunnelRectangle
theAtticInitialHorizontalTunnelRectangles =
    List.map (\( tunnel, opts ) -> tunnel) theAtticInitialHorizontalTunnelRectanglesWithOptions


theAtticInitialHorizontalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
theAtticInitialHorizontalTunnelRectanglesWithOptions =
    [ ( getCommonHorizontalTunnel 1 1, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 2, defaultHorizontalGreenDoorOptions )
    , ( getCommonHorizontalTunnel 1 3, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 2 1, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 2, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 3 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 3, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 4 1, defaultHorizontalYellowDoorOptions )
    , ( getCommonHorizontalTunnel 4 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 3, defaultHorizontalRedDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


theAtticCustomRoomRectangles : List RoomRectangle
theAtticCustomRoomRectangles =
    []


theAtticStairsTunnel : List TunnelRectangle
theAtticStairsTunnel =
    [ getVerticalTunnel 1 4 TunnelUp Nothing (Just (config_params.horizontal_wall_height + 1)) (Just SquareRoom) Nothing Nothing config_params
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
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction rec.roomType rec.shift rec.mbLocationShift config_params gridacc) grid lstairs
