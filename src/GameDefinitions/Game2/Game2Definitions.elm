module GameDefinitions.Game2.Game2Definitions exposing (initialModelFunc)

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameDefinitions.Common exposing (ItemCreationInfo, get_total_height, get_total_width, setAllAsUnexplored, setItemsInGrid)
import GameDefinitions.Game2.Basement as Basement
import GameDefinitions.Game2.Caverns as Caverns
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
import GameDefinitions.Game2.FirstFloor as FirstFloor
import GameDefinitions.Game2.GroundFloor as GroundFloor
import GameDefinitions.Game2.LastFloor as LastFloor
import GameDefinitions.Game2.TheAttic as TheAttic
import GameModel exposing (HoleInfo, RoomRectangle, RoomType(..), TeleporterInfo, TeleporterType(..), TunnelRectangle)
import Grid
import Item exposing (Item(..), KeyInfo)
import MapGen
import Thorns.Types


initialPlayer : Player
initialPlayer =
    let
        elem =
            "@"

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    Beings.playerCreationFunc elem "You"


initialEnemy : EnemyId -> Enemy
initialEnemy enemyid =
    let
        elem =
            "e" ++ String.fromInt enemyid

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    Beings.enemyCreationFunc elem enemyid ("enemy" ++ String.fromInt enemyid)



--( 92, 60 )


initialModelFunc : ( GameModel.Model, Bool, Bool )
initialModelFunc =
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
    -- GameModel.Model
    ( { player = { player | location = { x = 60, y = 36 } }
      , enemies =
            Dict.fromList
                [ ( 1, enemy )
                , ( 2, enemy2 )
                ]
      , otherCharacters = Dict.empty
      , level = GroundFloor.gridGroundFloor -- Grid.Grid Tile

      --, levers = levers --Dict LeverId LeverInfo
      , explored = setAllAsUnexplored GroundFloor.gridGroundFloor -- Grid.Grid Visibility
      , log = [ "you enter the dungeons Ground Floor " ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing
      , gameOfThornsModeisOn = False
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , y_display_anchor = 3 --Int   , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , window_width = common_window_width
      , window_height = common_window_height
      , total_width = get_total_width config_params 7
      , total_height = get_total_height config_params 9
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
          , { level = Caverns.gridCaverns
            , explored = setAllAsUnexplored Caverns.gridCaverns
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 11 -- caverns has 11 room columns
            , total_height = get_total_height config_params 7 -- 7 room rows
            }
          )
        , ( 1
          , { level = Basement.gridBasement
            , explored = setAllAsUnexplored Basement.gridBasement
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 6 -- basement has 11 room columns
            , total_height = get_total_height config_params 7 -- 7 room rows
            }
          )
        , ( 2
          , { level = GroundFloor.gridGroundFloor
            , explored = setAllAsUnexplored GroundFloor.gridGroundFloor
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 7 -- groundFloor has 7room columns
            , total_height = get_total_height config_params 9 -- 9 room rows
            }
          )
        , ( 3
          , { level = FirstFloor.gridFirstFloor
            , explored = setAllAsUnexplored FirstFloor.gridFirstFloor
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 6 -- firstFloor has 6room columns
            , total_height = get_total_height config_params 7 -- 7 room rows
            }
          )
        , ( 4
          , { level = TheAttic.gridTheAttic
            , explored = setAllAsUnexplored TheAttic.gridTheAttic
            , window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 4 -- theAttic has 4 room columns
            , total_height = get_total_height config_params 4 -- 4 room rows
            }
          )
        , ( 5
          , { level = LastFloor.gridLastFloor
            , explored = setAllAsUnexplored LastFloor.gridLastFloor
            , window_width = 18
            , window_height = 18
            , total_width = get_total_width config_params 17 -- theAttic has 17 room columns
            , total_height = get_total_height config_params 17 -- 17 room rows
            }
          )
        ]



{-

   -- CONFIG


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


   type alias TeleporterId =
       Int



   getTeleportersByFloorId : Int -> Dict TeleporterId TeleporterInfo -> Dict TeleporterId TeleporterInfo
   getTeleportersByFloorId floorId dTeleporters =
       Dict.filter (\k v -> v.floor_id == floorId) dTeleporters


   getTeleportersCoordsAndTeleportersInfo : TeleporterInfo -> ( ( Int, Int ), TeleporterInfo )
   getTeleportersCoordsAndTeleportersInfo teleporterInfo =
       let
           ( x_coord, y_coord ) =
               get_xy_coords_from_room_row_col teleporterInfo.room_row_nr teleporterInfo.room_col_nr teleporterInfo.room_type teleporterInfo.position_in_room

           ( x_c, y_c ) =
               case teleporterInfo.teleporterType of
                   Clock ->
                       if String.toLower teleporterInfo.position_in_room == "up" || String.toLower teleporterInfo.position_in_room == "down" then
                           ( x_coord - 2, y_coord )

                       else
                           ( x_coord, y_coord )

                   _ ->
                       ( x_coord, y_coord )
       in
       ( ( x_c, y_c ), teleporterInfo )


   get_xy_coords_from_room_row_col : Int -> Int -> RoomType -> String -> ( Int, Int )
   get_xy_coords_from_room_row_col row_nr col_nr rtype pos_in_room =
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
                   ( top_left_x + room_width // 2, top_left_y - 1 )

               else if String.toLower pos_in_room == "down" then
                   ( top_left_x + room_width // 2, top_left_y + room_height )

               else if String.toLower pos_in_room == "left" then
                   ( top_left_x - 1, top_left_y + room_height // 2 )

               else
                   -- String.toLower pos_in_room == "right" then
                   ( top_left_x + room_width, top_left_y + room_height // 2 )
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



   defaultNoDoorOptions : GameModel.DoorWallOptions
   defaultNoDoorOptions =
       { left = GameModel.NoDoorNoWall
       , top = GameModel.NoDoorNoWall
       , right = GameModel.NoDoorNoWall
       , bottom = GameModel.NoDoorNoWall
       }


   defaultHorizontalOpenDoorOptions : GameModel.DoorWallOptions
   defaultHorizontalOpenDoorOptions =
       { left = GameModel.UseDoor (GameModel.defaultOpenDoorInfo GameModel.DoorToTheRight)
       , top = GameModel.NoDoorNoWall
       , right = GameModel.UseDoor (GameModel.defaultOpenDoorInfo GameModel.DoorToTheLeft)
       , bottom = GameModel.NoDoorNoWall
       }


   defaultVerticalOpenDoorOptions : GameModel.DoorWallOptions
   defaultVerticalOpenDoorOptions =
       { left = GameModel.NoDoorNoWall
       , top = GameModel.UseDoor (GameModel.defaultOpenDoorInfo GameModel.DoorToDown)
       , right = GameModel.NoDoorNoWall
       , bottom = GameModel.UseDoor (GameModel.defaultOpenDoorInfo GameModel.DoorToUp)
       }


   defaultHorizontalBlueDoorOptions : GameModel.DoorWallOptions
   defaultHorizontalBlueDoorOptions =
       { left = GameModel.UseDoor (GameModel.defaultBlueDoorInfo GameModel.DoorToTheRight)
       , top = GameModel.NoDoorNoWall
       , right = GameModel.UseDoor (GameModel.defaultBlueDoorInfo GameModel.DoorToTheLeft)
       , bottom = GameModel.NoDoorNoWall
       }


   defaultVerticalBlueDoorOptions : GameModel.DoorWallOptions
   defaultVerticalBlueDoorOptions =
       { left = GameModel.NoDoorNoWall
       , top = GameModel.UseDoor (GameModel.defaultBlueDoorInfo GameModel.DoorToDown)
       , right = GameModel.NoDoorNoWall
       , bottom = GameModel.UseDoor (GameModel.defaultBlueDoorInfo GameModel.DoorToUp)
       }


   defaultHorizontalRedDoorOptions : GameModel.DoorWallOptions
   defaultHorizontalRedDoorOptions =
       { left = GameModel.UseDoor (GameModel.defaultRedDoorInfo GameModel.DoorToTheRight)
       , top = GameModel.NoDoorNoWall
       , right = GameModel.UseDoor (GameModel.defaultRedDoorInfo GameModel.DoorToTheLeft)
       , bottom = GameModel.NoDoorNoWall
       }


   defaultVerticalRedDoorOptions : GameModel.DoorWallOptions
   defaultVerticalRedDoorOptions =
       { left = GameModel.NoDoorNoWall
       , top = GameModel.UseDoor (GameModel.defaultRedDoorInfo GameModel.DoorToDown)
       , right = GameModel.NoDoorNoWall
       , bottom = GameModel.UseDoor (GameModel.defaultRedDoorInfo GameModel.DoorToUp)
       }


   defaultHorizontalGreenDoorOptions : GameModel.DoorWallOptions
   defaultHorizontalGreenDoorOptions =
       { left = GameModel.UseDoor (GameModel.defaultGreenDoorInfo GameModel.DoorToTheRight)
       , top = GameModel.NoDoorNoWall
       , right = GameModel.UseDoor (GameModel.defaultGreenDoorInfo GameModel.DoorToTheLeft)
       , bottom = GameModel.NoDoorNoWall
       }


   defaultVerticalGreenDoorOptions : GameModel.DoorWallOptions
   defaultVerticalGreenDoorOptions =
       { left = GameModel.NoDoorNoWall
       , top = GameModel.UseDoor (GameModel.defaultGreenDoorInfo GameModel.DoorToDown)
       , right = GameModel.NoDoorNoWall
       , bottom = GameModel.UseDoor (GameModel.defaultGreenDoorInfo GameModel.DoorToUp)
       }


   defaultHorizontalYellowDoorOptions : GameModel.DoorWallOptions
   defaultHorizontalYellowDoorOptions =
       { left = GameModel.UseDoor (GameModel.defaultYellowDoorInfo GameModel.DoorToTheRight)
       , top = GameModel.NoDoorNoWall
       , right = GameModel.UseDoor (GameModel.defaultYellowDoorInfo GameModel.DoorToTheLeft)
       , bottom = GameModel.NoDoorNoWall
       }


   defaultVerticalYellowDoorOptions : GameModel.DoorWallOptions
   defaultVerticalYellowDoorOptions =
       { left = GameModel.NoDoorNoWall
       , top = GameModel.UseDoor (GameModel.defaultYellowDoorInfo GameModel.DoorToDown)
       , right = GameModel.NoDoorNoWall
       , bottom = GameModel.UseDoor (GameModel.defaultYellowDoorInfo GameModel.DoorToUp)
       }


   type alias ItemId =
       Int


   getItemsByFloorId : Int -> Dict ItemId ItemCreationInfo -> Dict ItemId ItemCreationInfo
   getItemsByFloorId floorId dItems =
       Dict.filter (\k v -> v.floor_id == floorId) dItems



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

-}
