module GameDefinitions.Game2.Caverns exposing (gridCaverns)

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


gridCaverns : Grid.Grid GameModel.Tile
gridCaverns =
    GameDefinitions.Common.gridInitializer 7 11 config_params
        |> addCavernsCustomRoomsAndTunnels


addCavernsCustomRoomsAndTunnels : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
addCavernsCustomRoomsAndTunnels grid =
    grid
        |> MapGen.listRoomRectangleToGridFunc cavernsInitialRoomRectangles
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc cavernsInitialHorizontalTunnelRectanglesWithOptions
        |> MapGen.listTunnelRectangleToGridFunc cavernsStairsTunnel
        |> MapGen.listTunnelRectangleWithOptionsToGridFunc cavernsInitialVerticalTunnelRectanglesWithOptions
        |> MapGen.createWallBoundaries (cavernsInitialHorizontalTunnelRectangles ++ cavernsStairsTunnel ++ cavernsInitialRoomRectangles ++ cavernsInitialVerticalTunnelRectangles)
        --|> make sure if cell width x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
        |> addCavernsStairs
        |> setLandingTargetsInGrid cavernsLandingTargets
        |> setTeleportersInGrid config_params cavernsTeleporters
        |> setItemsInGrid config_params cavernsItems
        |> MapGen.transformFloorToWallOnDisplayBoundaries
        |> MapGen.correctSomeWallCorners



-- ITEMS in rooms


cavernsItems : Dict ItemId ItemCreationInfo
cavernsItems =
    getItemsByFloorId caverns_floor_id itemCreationDict


cavernsTeleporters : Dict TeleporterId TeleporterInfo
cavernsTeleporters =
    getTeleportersByFloorId caverns_floor_id teleporterInfoDict


cavernsLandingTargets : Dict TargetId LandingTargetInfo
cavernsLandingTargets =
    getLandingTargetsByFloorId caverns_floor_id landingTargetsDict



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
        |> List.map (\xfunc -> xfunc config_params)


cavernsInitialHorizontalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
cavernsInitialHorizontalTunnelRectanglesWithOptions =
    [ ( getCommonHorizontalTunnel 1 7, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 8, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 1 9, defaultHorizontalGreenDoorOptions )
    , ( getCommonHorizontalTunnel 1 10, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 2 1, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 2 4, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 3 6, defaultHorizontalBlueDoorOptions )
    , ( getCommonHorizontalTunnel 3 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 3 7, defaultHorizontalRedDoorOptions )
    , ( getCommonHorizontalTunnel 3 8, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 4 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 8, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 9, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 4 10, defaultHorizontalRedDoorOptions )

    --
    , ( getCommonHorizontalTunnel 5 2, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 3, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 4, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 5 5, defaultHorizontalGreenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 6 5, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 6, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 7, defaultHorizontalBlueDoorOptions )
    , ( getCommonHorizontalTunnel 6 8, defaultHorizontalOpenDoorOptions )
    , ( getCommonHorizontalTunnel 6 9, defaultHorizontalOpenDoorOptions )

    --
    , ( getCommonHorizontalTunnel 7 6, defaultHorizontalOpenDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


cavernsInitialHorizontalTunnelRectangles : List TunnelRectangle
cavernsInitialHorizontalTunnelRectangles =
    List.map (\( tun, opt ) -> tun) cavernsInitialHorizontalTunnelRectanglesWithOptions


cavernsInitialVerticalTunnelRectanglesWithOptions : List ( TunnelRectangle, GameModel.DoorWallOptions )
cavernsInitialVerticalTunnelRectanglesWithOptions =
    [ ( getCommonVerticalTunnel 2 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 4 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 2, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 2, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 3, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 3, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 2 5, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 5, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 4 6, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 6, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 6, defaultVerticalGreenDoorOptions )

    --
    , ( getCommonVerticalTunnel 1 7, defaultVerticalRedDoorOptions )
    , ( getCommonVerticalTunnel 2 7, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 7, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 7, defaultVerticalOpenDoorOptions )

    --
    , ( getCommonVerticalTunnel 1 9, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 2 9, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 3 9, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 4 9, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 5 9, defaultVerticalOpenDoorOptions )
    , ( getCommonVerticalTunnel 6 9, defaultVerticalOpenDoorOptions )
    ]
        |> List.map (\( xfunc, y ) -> ( xfunc config_params, y ))


cavernsInitialVerticalTunnelRectangles : List TunnelRectangle
cavernsInitialVerticalTunnelRectangles =
    List.map (\( tunnel, opt ) -> tunnel) cavernsInitialVerticalTunnelRectanglesWithOptions


cavernsStairsTunnel : List TunnelRectangle
cavernsStairsTunnel =
    [ --getHorizontalTunnel 4 8 TunnelToTheLeft (Just (vertical_wall_width + horizontal_space_between_rooms)) Nothing Nothing Nothing Nothing
      getHorizontalTunnel 4 8 TunnelToTheLeft (Just (config_params.vertical_wall_width + 1)) Nothing Nothing Nothing Nothing config_params
    , getVerticalTunnel 7 2 TunnelDown Nothing (Just (config_params.horizontal_wall_height + 1)) Nothing Nothing Nothing config_params
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
    List.foldl (\rec gridacc -> getStairsOnRoom rec.room_row rec.room_col rec.stairsId rec.toFloorId rec.toStairsId rec.direction Nothing rec.shift rec.mbLocationShift config_params gridacc) grid lstairs
