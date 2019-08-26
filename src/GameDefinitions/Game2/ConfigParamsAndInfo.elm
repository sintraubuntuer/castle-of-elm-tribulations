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
        , createHoleInfo
        , createHoleInfoWithLocation
        , createTeleporterInfoWithLocation
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
        ( RoomRectangle
        , RoomType(..)
        , TunnelRectangle
        )
import Item exposing (Item(..), KeyInfo)
import Tile
    exposing
        ( TeleporterInfo
        , TeleporterType(..)
        )



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


holesDict : Dict HoleId HoleInfoWithLocation
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
        ([ ( 1, createHoleInfoWithLocation 1 groundFloor_id 1 1 5 HorizontalRoom 1 )
         , ( 2, createHoleInfoWithLocation 2 groundFloor_id 1 4 8 HorizontalRoom 2 )
         , ( 3, createHoleInfoWithLocation 3 groundFloor_id 3 5 4 SquareRoom 3 )
         , ( 4, createHoleInfoWithLocation 4 basement_floor_id 5 5 5 SquareRoom 4 )
         , ( 5, createHoleInfoWithLocation 5 firstFloor_id 3 4 6 SquareRoom 5 )
         , ( 6, createHoleInfoWithLocation 6 firstFloor_id 3 3 5 VerticalRoom 6 )
         , ( 7, createHoleInfoWithLocation 7 firstFloor_id 5 3 2 VerticalRoom 7 )
         , ( 8, createHoleInfoWithLocation 8 theAttic_id 1 2 8 SquareRoom 8 )
         , ( 9, createHoleInfoWithLocation 9 theAttic_id 3 4 5 SquareRoom 9 )
         , ( 10, createHoleInfoWithLocation 10 theAttic_id 2 1 5 SquareRoom 10 )
         , ( 11, createHoleInfoWithLocation 11 theAttic_id 2 3 6 SquareRoom 11 )
         ]
            |> List.map (\( x, func_y ) -> ( x, func_y config_params ))
        )


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


teleporterInfoDict : Dict TeleporterId TeleporterInfoWithLocation
teleporterInfoDict =
    Dict.fromList
        [ ( 1, createTeleporterInfoWithLocation 1 caverns_floor_id Barrel 2 5 SquareRoom "right" 2 ( 1, 0 ) )
        , ( 2, createTeleporterInfoWithLocation 2 caverns_floor_id Barrel 1 7 SquareRoom "left" 1 ( -1, 0 ) )
        , ( 3, createTeleporterInfoWithLocation 3 caverns_floor_id Barrel 3 5 SquareRoom "down" 4 ( 0, 1 ) )
        , ( 4, createTeleporterInfoWithLocation 4 caverns_floor_id Barrel 5 5 SquareRoom "up" 3 ( 0, -1 ) )
        , ( 5, createTeleporterInfoWithLocation 5 caverns_floor_id Barrel 1 11 SquareRoom "down" 6 ( 0, 1 ) )
        , ( 6, createTeleporterInfoWithLocation 6 caverns_floor_id Barrel 4 11 SquareRoom "up" 5 ( 0, -1 ) )
        , ( 7, createTeleporterInfoWithLocation 7 caverns_floor_id BookCase 4 7 VerticalRoom "down" 8 ( 0, 1 ) )
        , ( 8, createTeleporterInfoWithLocation 8 caverns_floor_id BookCase 6 7 SquareRoom "up" 7 ( 0, -1 ) )
        , ( 9, createTeleporterInfoWithLocation 9 caverns_floor_id BookCase 7 7 SquareRoom "right" 10 ( 0, -1 ) )
        , ( 10, createTeleporterInfoWithLocation 10 caverns_floor_id BookCase 6 10 SquareRoom "down" 9 ( -1, 0 ) )
        , ( 11, createTeleporterInfoWithLocation 11 caverns_floor_id Clock 7 2 SquareRoom "right" 12 ( 1, 0 ) )
        , ( 12, createTeleporterInfoWithLocation 12 caverns_floor_id Clock 7 9 SquareRoom "left" 11 ( -1, 0 ) )
        , ( 13, createTeleporterInfoWithLocation 13 basement_floor_id BookCase 2 1 SquareRoom "down" 14 ( 0, 1 ) )
        , ( 14, createTeleporterInfoWithLocation 14 basement_floor_id BookCase 6 1 SquareRoom "up" 13 ( 0, -1 ) )
        , ( 15, createTeleporterInfoWithLocation 15 basement_floor_id Clock 1 2 SquareRoom "right" 16 ( 1, 0 ) )
        , ( 16, createTeleporterInfoWithLocation 16 basement_floor_id Clock 1 5 SquareRoom "left" 15 ( -1, 0 ) )
        , ( 17, createTeleporterInfoWithLocation 17 groundFloor_id Barrel 3 3 SquareRoom "down" 18 ( 0, 1 ) )
        , ( 18, createTeleporterInfoWithLocation 18 groundFloor_id Barrel 5 3 SquareRoom "up" 17 ( 0, -1 ) )
        , ( 19, createTeleporterInfoWithLocation 19 groundFloor_id Barrel 6 6 VerticalRoom "left" 20 ( -1, 0 ) )
        , ( 20, createTeleporterInfoWithLocation 20 firstFloor_id Barrel 6 4 SquareRoom "right" 19 ( 1, 0 ) )
        , ( 21, createTeleporterInfoWithLocation 21 groundFloor_id BookCase 1 5 HorizontalRoom "down" 22 ( 0, 1 ) )
        , ( 22, createTeleporterInfoWithLocation 22 groundFloor_id BookCase 7 5 HorizontalRoom "up" 21 ( 0, -1 ) )
        , ( 23, createTeleporterInfoWithLocation 23 groundFloor_id BookCase 3 4 SquareRoom "down" 24 ( 0, 1 ) )
        , ( 24, createTeleporterInfoWithLocation 24 groundFloor_id BookCase 5 4 SquareRoom "up" 23 ( 0, -1 ) )
        , ( 25, createTeleporterInfoWithLocation 25 groundFloor_id Clock 1 2 SquareRoom "down" 26 ( 0, 1 ) )
        , ( 26, createTeleporterInfoWithLocation 26 groundFloor_id Clock 7 2 SquareRoom "up" 25 ( 0, -1 ) )
        , ( 27, createTeleporterInfoWithLocation 27 groundFloor_id Clock 1 6 SquareRoom "down" 28 ( 0, 1 ) )
        , ( 28, createTeleporterInfoWithLocation 28 groundFloor_id Clock 7 6 SquareRoom "up" 27 ( 0, -1 ) )
        , ( 29, createTeleporterInfoWithLocation 29 firstFloor_id Clock 5 3 VerticalRoom "right" 30 ( 1, 0 ) )
        , ( 30, createTeleporterInfoWithLocation 30 firstFloor_id Clock 4 4 VerticalRoom "left" 29 ( -1, 0 ) )
        , ( 31, createTeleporterInfoWithLocation 31 theAttic_id Clock 1 1 SquareRoom "down" 32 ( 0, 1 ) )
        , ( 32, createTeleporterInfoWithLocation 32 theAttic_id Clock 2 1 SquareRoom "up" 31 ( 0, -1 ) )
        ]
