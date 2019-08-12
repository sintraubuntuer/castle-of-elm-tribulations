module GameDefinitions.Game2.ConfigParamsAndInfo exposing
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

import Dict exposing (Dict)
import GameDefinitions.Common
    exposing
        ( ConfigParams
        , HoleId
        , HorizontalTunnelOrientation(..)
        , ItemCreationInfo
        , ItemId
        , LandingTargetInfo
        , StairsOrientation(..)
        , TargetId
        , TeleporterId
        , VerticalTunnelOrientation(..)
        , createHoleInfo
        , createTeleporterInfo
        , defaultHorizontalGreenDoorOptions
        , defaultHorizontalOpenDoorOptions
        , defaultHorizontalRedDoorOptions
        , defaultHorizontalYellowDoorOptions
        , defaultNoDoorOptions
        , defaultVerticalBlueDoorOptions
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
        , get_room_position_nr
        , setHolesInGrid
        , setItemsInGrid
        , setLandingTargetsInGrid
        , setTeleportersInGrid
        )
import GameModel
    exposing
        ( HoleInfo
        , RoomRectangle
        , RoomType(..)
        , TeleporterInfo
        , TeleporterType(..)
        , TunnelRectangle
        )
import Item exposing (Item(..), KeyInfo)



-- CONFIG


square_room_side_ =
    7


config_params : ConfigParams
config_params =
    { square_room_side = square_room_side_
    , small_width_room_width = square_room_side_ - 2
    , small_width_room_height = square_room_side_
    , small_height_room_width = square_room_side_
    , small_height_room_height = square_room_side_ - 2
    , horizontal_wall_height = 1
    , vertical_wall_width = 1
    , horizontal_space_between_rooms = 1
    , vertical_space_between_rooms = 1
    , border_top_height = 1
    , border_left_width = 1
    , border_bottom_height = 4
    , border_right_width = 4
    }


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


lastFloor_id : Int
lastFloor_id =
    5


holesDict : Dict HoleId GameModel.HoleInfo
holesDict =
    let
        ( x1, y1 ) =
            get_room_position_nr 1 1 5 HorizontalRoom config_params

        ( x2, y2 ) =
            get_room_position_nr 1 4 8 HorizontalRoom config_params

        ( x3, y3 ) =
            get_room_position_nr 3 5 4 SquareRoom config_params

        ( x4, y4 ) =
            get_room_position_nr 5 5 5 SquareRoom config_params

        ( x5, y5 ) =
            get_room_position_nr 3 4 6 SquareRoom config_params

        ( x6, y6 ) =
            get_room_position_nr 3 3 5 VerticalRoom config_params

        ( x7, y7 ) =
            get_room_position_nr 5 3 2 VerticalRoom config_params

        ( x8, y8 ) =
            get_room_position_nr 1 2 8 SquareRoom config_params

        ( x9, y9 ) =
            get_room_position_nr 3 4 5 SquareRoom config_params

        ( x10, y10 ) =
            get_room_position_nr 2 1 5 SquareRoom config_params

        ( x11, y11 ) =
            get_room_position_nr 2 3 6 SquareRoom config_params
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


itemCreationDict : Dict ItemId ItemCreationInfo
itemCreationDict =
    Dict.fromList
        [ ( 1, ItemCreationInfo 1 (Key { keyColor = "blue" }) caverns_floor_id 4 3 SquareRoom 5 Nothing )
        , ( 2, ItemCreationInfo 2 (Key { keyColor = "yellow" }) basement_floor_id 4 3 SquareRoom 7 (Just ( 5, 0 )) )
        , ( 3, ItemCreationInfo 3 (Key { keyColor = "red" }) groundFloor_id 1 2 SquareRoom 3 Nothing )
        , ( 4, ItemCreationInfo 4 (Key { keyColor = "green" }) groundFloor_id 5 6 SquareRoom 5 Nothing )
        ]


landingTargetsDict : Dict TargetId LandingTargetInfo
landingTargetsDict =
    let
        ( x1, y1 ) =
            get_room_position_nr 2 1 5 SquareRoom config_params

        ( x2, y2 ) =
            get_room_position_nr 4 3 5 SquareRoom config_params

        ( x3, y3 ) =
            get_room_position_nr 6 5 5 SquareRoom config_params

        ( x4, y4 ) =
            get_room_position_nr 3 5 5 SquareRoom config_params

        ( x5, y5 ) =
            get_room_position_nr 3 5 6 SquareRoom config_params

        ( x6, y6 ) =
            get_room_position_nr 3 4 5 SquareRoom config_params

        ( x7, y7 ) =
            get_room_position_nr 5 4 2 SquareRoom config_params

        ( x8, y8 ) =
            get_room_position_nr 5 3 8 VerticalRoom config_params

        ( x9, y9 ) =
            get_room_position_nr 7 6 5 SquareRoom config_params

        ( x10, y10 ) =
            get_room_position_nr 6 2 5 SquareRoom config_params

        ( x11, y11 ) =
            get_room_position_nr 6 4 6 SquareRoom config_params
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
