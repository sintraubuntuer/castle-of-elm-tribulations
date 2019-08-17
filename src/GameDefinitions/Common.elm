module GameDefinitions.Common exposing
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
    , dimensions
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
    , get_room_position_nr
    , get_total_height
    , get_total_width
    , gridInitializer
    , initialEnemy
    , initialModelFunc
    , initialPlayer
    , setAllAsUnexplored
    , setHolesInGrid
    , setItemsInGrid
    , setLandingTargetsInGrid
    , setTeleportersInGrid
    )

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameModel exposing (HoleInfo, RoomRectangle, RoomType(..), TeleporterInfo, TeleporterType(..), TunnelRectangle)
import Grid
import Item exposing (Item(..), KeyInfo)
import Thorns.Types


type alias HoleId =
    Int


type alias TeleporterId =
    Int


type alias ItemId =
    Int


gridInitializer : Int -> Int -> ConfigParams -> Grid.Grid GameModel.Tile
gridInitializer nr_rows nr_cols config_params =
    Grid.initialize { width = get_total_width config_params nr_cols, height = get_total_height config_params nr_rows } GameModel.NoTileYet


setAllAsUnexplored : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Visibility
setAllAsUnexplored level =
    let
        grid =
            Grid.toList level
    in
    List.map (\row -> List.map (\_ -> GameModel.Unexplored) row) grid |> Grid.fromList


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


initialEnemy : EnemyId -> String -> Int -> Enemy
initialEnemy enemyid species floor_id =
    let
        elem =
            "e" ++ String.fromInt enemyid

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    Beings.enemyCreationFunc elem enemyid ("enemy" ++ String.fromInt enemyid) species floor_id


dimensions : ( Int, Int )
dimensions =
    ( 10, 10 )


initialModelFunc : ( GameModel.Model, Bool )
initialModelFunc =
    let
        player =
            initialPlayer

        enemy =
            initialEnemy 1 "ghost" theFloorId

        enemy2 =
            initialEnemy 2 "ghost" theFloorId

        levers =
            Dict.empty

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            -- MapGen.randomCave dimensions
            gridInitializer w h cParams

        theFloorId =
            1

        createRandomMap =
            False
    in
    -- GameModel.Model
    ( { player = player
      , enemies =
            Dict.fromList
                [ ( 1, enemy )
                , ( 2, enemy2 )
                ]
      , otherCharacters = Dict.empty
      , level = firstMap -- Grid.Grid Tile
      , explored = setAllAsUnexplored firstMap -- Grid.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing
      , gameOfThornsModeisOn = False
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int
      , y_display_anchor = 3 --Int
      , window_width = 10
      , window_height = 10
      , total_width = Tuple.first dimensions
      , total_height = Tuple.second dimensions
      , currentDisplay = GameModel.DisplayRegularGame
      , displayStatsOverlay = False
      , displayInventory = False
      , showBlood = True
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = Dict.empty
      , currentFloorId = theFloorId
      , started = False
      }
    , createRandomMap
    )



-- CONFIG PARAMS


square_room_side_ =
    7


type alias ConfigParams =
    { square_room_side : Int
    , small_width_room_width : Int
    , small_width_room_height : Int
    , small_height_room_width : Int
    , small_height_room_height : Int
    , horizontal_wall_height : Int
    , vertical_wall_width : Int
    , horizontal_space_between_rooms : Int
    , vertical_space_between_rooms : Int
    , border_top_height : Int
    , border_left_width : Int
    , border_bottom_height : Int
    , border_right_width : Int
    }


cParams : ConfigParams
cParams =
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



-- TYPES AND FUNCS


type alias TargetId =
    Int


type alias LandingTargetInfo =
    { target_id : Int
    , floor_id : Int
    , x : Int
    , y : Int
    , mbLocationShift : Maybe ( Int, Int )
    }


type alias ItemCreationInfo =
    { item_id : Int
    , item : Item
    , floor_id : Int
    , room_row_nr : Int
    , room_col_nr : Int
    , room_type : RoomType
    , position_in_room : Int --(in WallUp , L R or D )
    , mbLocationShift : Maybe ( Int, Int )
    }


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


getItemsByFloorId : Int -> Dict ItemId ItemCreationInfo -> Dict ItemId ItemCreationInfo
getItemsByFloorId floorId dItems =
    Dict.filter (\k v -> v.floor_id == floorId) dItems


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


getCommonHorizontalTunnel : Int -> Int -> ConfigParams -> TunnelRectangle
getCommonHorizontalTunnel row_nr col_nr config_params =
    getHorizontalTunnel row_nr col_nr TunnelToTheRight Nothing Nothing Nothing Nothing Nothing config_params


getHorizontalTunnel : Int -> Int -> HorizontalTunnelOrientation -> Maybe Int -> Maybe Int -> Maybe RoomType -> Maybe RoomType -> Maybe ( Int, Int ) -> ConfigParams -> TunnelRectangle
getHorizontalTunnel row_nr col_nr orientation mbWidth mbHeight mbFromRoomType mbToRoomType mbShift config_params =
    let
        tunnel_width =
            case mbWidth of
                Just awidth ->
                    awidth

                Nothing ->
                    (2 * config_params.vertical_wall_width + config_params.horizontal_space_between_rooms) + addOneIfRoomTypeVertical mbFromRoomType + addOneIfRoomTypeVertical mbToRoomType

        ( x_shift, y_shift ) =
            mbShift |> Maybe.withDefault ( 0, 0 )

        ( top_left_x, top_left_y ) =
            case orientation of
                TunnelToTheLeft ->
                    ( square_room_top_left_x config_params row_nr col_nr - tunnel_width + x_shift + addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y config_params row_nr col_nr + config_params.square_room_side // 2 + y_shift )

                TunnelToTheRight ->
                    ( square_room_top_left_x config_params row_nr col_nr + config_params.square_room_side + x_shift - addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y config_params row_nr col_nr + config_params.square_room_side // 2 + y_shift )

        tunnel_height =
            mbHeight |> Maybe.withDefault 1
    in
    TunnelRectangle top_left_x top_left_y tunnel_width tunnel_height


getCommonVerticalTunnel : Int -> Int -> ConfigParams -> TunnelRectangle
getCommonVerticalTunnel row_nr col_nr config_params =
    getVerticalTunnel row_nr col_nr TunnelDown Nothing Nothing Nothing Nothing Nothing config_params


getVerticalTunnel : Int -> Int -> VerticalTunnelOrientation -> Maybe Int -> Maybe Int -> Maybe RoomType -> Maybe RoomType -> Maybe ( Int, Int ) -> ConfigParams -> TunnelRectangle
getVerticalTunnel row_nr col_nr orientation mbWidth mbHeight mbFromRoomType mbToRoomType mbShift config_params =
    let
        tunnel_height =
            --mbHeight |> Maybe.withDefault (2 * horizontal_wall_height + vertical_space_between_rooms)
            case mbHeight of
                Just aheight ->
                    aheight

                Nothing ->
                    (2 * config_params.horizontal_wall_height + config_params.vertical_space_between_rooms) + addOneIfRoomTypeHorizontal mbFromRoomType + addOneIfRoomTypeHorizontal mbToRoomType

        tunnel_width =
            mbWidth |> Maybe.withDefault 1

        ( x_shift, y_shift ) =
            mbShift |> Maybe.withDefault ( 0, 0 )

        ( top_left_x, top_left_y ) =
            case orientation of
                TunnelUp ->
                    ( square_room_top_left_x config_params row_nr col_nr + config_params.square_room_side // 2 + x_shift, square_room_top_left_y config_params row_nr col_nr - tunnel_height + y_shift + addOneIfRoomTypeHorizontal mbFromRoomType )

                TunnelDown ->
                    ( square_room_top_left_x config_params row_nr col_nr + config_params.square_room_side // 2 + x_shift, square_room_top_left_y config_params row_nr col_nr + config_params.square_room_side + y_shift - addOneIfRoomTypeHorizontal mbFromRoomType )
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


get_total_width : ConfigParams -> Int -> Int
get_total_width config_params nr_rooms_per_row =
    config_params.border_left_width + config_params.border_right_width + nr_rooms_per_row * (config_params.square_room_side + 2 * config_params.vertical_wall_width) + (nr_rooms_per_row - 1) * config_params.horizontal_space_between_rooms


get_total_height : ConfigParams -> Int -> Int
get_total_height config_params nr_rooms_per_column =
    config_params.border_top_height + config_params.border_bottom_height + nr_rooms_per_column * (config_params.square_room_side + 2 * config_params.horizontal_wall_height) + (nr_rooms_per_column - 1) * config_params.vertical_space_between_rooms


square_room_top_left_y : ConfigParams -> Int -> Int -> Int
square_room_top_left_y config_params row_nr col_nr =
    config_params.border_top_height + (row_nr * config_params.horizontal_wall_height + (row_nr - 1) * (config_params.square_room_side + config_params.horizontal_wall_height + config_params.vertical_space_between_rooms)) + 1


square_room_top_left_x : ConfigParams -> Int -> Int -> Int
square_room_top_left_x config_params row_nr col_nr =
    config_params.border_left_width + (col_nr * config_params.vertical_wall_width + (col_nr - 1) * (config_params.square_room_side + config_params.vertical_wall_width + config_params.horizontal_space_between_rooms)) + 1


small_width_room_top_left_y : ConfigParams -> Int -> Int -> Int
small_width_room_top_left_y config_params row_nr col_nr =
    square_room_top_left_y config_params row_nr col_nr


small_width_room_top_left_x : ConfigParams -> Int -> Int -> Int
small_width_room_top_left_x config_params row_nr col_nr =
    square_room_top_left_x config_params row_nr col_nr + 1


small_height_room_top_left_y : ConfigParams -> Int -> Int -> Int
small_height_room_top_left_y config_params row_nr col_nr =
    square_room_top_left_y config_params row_nr col_nr + 1


small_height_room_top_left_x : ConfigParams -> Int -> Int -> Int
small_height_room_top_left_x config_params row_nr col_nr =
    square_room_top_left_x config_params row_nr col_nr


getRoom : Int -> Int -> RoomType -> ConfigParams -> RoomRectangle
getRoom row_nr col_nr room_type config_params =
    let
        ( top_left_x, top_left_y ) =
            case room_type of
                SquareRoom ->
                    ( square_room_top_left_x config_params row_nr col_nr, square_room_top_left_y config_params row_nr col_nr )

                HorizontalRoom ->
                    ( small_height_room_top_left_x config_params row_nr col_nr, small_height_room_top_left_y config_params row_nr col_nr )

                VerticalRoom ->
                    ( small_width_room_top_left_x config_params row_nr col_nr, small_width_room_top_left_y config_params row_nr col_nr )

        ( room_width, room_height ) =
            case room_type of
                SquareRoom ->
                    ( config_params.square_room_side, config_params.square_room_side )

                HorizontalRoom ->
                    ( config_params.small_height_room_width, config_params.small_height_room_height )

                VerticalRoom ->
                    ( config_params.small_width_room_width, config_params.small_width_room_height )
    in
    RoomRectangle top_left_x top_left_y room_width room_height


getCustomRoom : Int -> Int -> Int -> Int -> Int -> Int -> ConfigParams -> RoomRectangle
getCustomRoom row_nr col_nr shift_x shift_y rwidth rheight config_params =
    let
        room =
            getRoom row_nr col_nr SquareRoom config_params

        new_room =
            RoomRectangle (room.top_left_x + shift_x) (room.top_left_y + shift_y) rwidth rheight
    in
    new_room


getStairsOnRoom : Int -> Int -> Int -> Int -> Int -> StairsOrientation -> Maybe RoomType -> ( Int, Int ) -> Maybe ( Int, Int ) -> ConfigParams -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
getStairsOnRoom row_nr col_nr stairsId toFloorId toStairsId orientation mbFromRoomType shiftOnDestinationTuple mbShiftLocationTuple config_params grid =
    let
        tunnel_width =
            --vertical_wall_width + horizontal_space_between_rooms
            config_params.vertical_wall_width + 1

        tunnel_height =
            --horizontal_wall_height + vertical_space_between_rooms
            config_params.horizontal_wall_height + 1

        ( loc_x_shift, loc_y_shift ) =
            mbShiftLocationTuple |> Maybe.withDefault ( 0, 0 )

        ( top_left_x, top_left_y ) =
            case orientation of
                StairsToTheLeft ->
                    ( square_room_top_left_x config_params row_nr col_nr - tunnel_width + loc_x_shift + addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y config_params row_nr col_nr + config_params.square_room_side // 2 + loc_y_shift )

                StairsToTheRight ->
                    ( square_room_top_left_x config_params row_nr col_nr + config_params.square_room_side + tunnel_width - 1 + loc_x_shift - addOneIfRoomTypeVertical mbFromRoomType, square_room_top_left_y config_params row_nr col_nr + config_params.square_room_side // 2 + loc_y_shift )

                StairsUp ->
                    ( square_room_top_left_x config_params row_nr col_nr + config_params.square_room_side // 2 + loc_x_shift, square_room_top_left_y config_params row_nr col_nr - tunnel_height + loc_y_shift + addOneIfRoomTypeHorizontal mbFromRoomType )

                StairsDown ->
                    ( square_room_top_left_x config_params row_nr col_nr + config_params.square_room_side // 2 + loc_x_shift, square_room_top_left_y config_params row_nr col_nr + config_params.square_room_side + tunnel_height - 1 + loc_y_shift - addOneIfRoomTypeHorizontal mbFromRoomType )

        tileStairs =
            GameModel.Stairs (GameModel.StairsInfo stairsId toFloorId toStairsId shiftOnDestinationTuple False GameModel.Unexplored)
    in
    Grid.set (Grid.Coordinate top_left_x top_left_y) tileStairs grid


get_room_width : RoomType -> ConfigParams -> Int
get_room_width roomType config_params =
    case roomType of
        SquareRoom ->
            config_params.square_room_side

        HorizontalRoom ->
            config_params.small_height_room_width

        VerticalRoom ->
            config_params.small_width_room_width


get_room_height : RoomType -> ConfigParams -> Int
get_room_height roomType config_params =
    case roomType of
        SquareRoom ->
            config_params.square_room_side

        HorizontalRoom ->
            config_params.small_height_room_height

        VerticalRoom ->
            config_params.small_width_room_height


get_room_top_left_x : Int -> Int -> RoomType -> ConfigParams -> Int
get_room_top_left_x row_nr col_nr roomType config_params =
    case roomType of
        SquareRoom ->
            square_room_top_left_x config_params row_nr col_nr

        HorizontalRoom ->
            small_height_room_top_left_x config_params row_nr col_nr

        VerticalRoom ->
            small_width_room_top_left_x config_params row_nr col_nr


get_room_top_left_y : Int -> Int -> RoomType -> ConfigParams -> Int
get_room_top_left_y row_nr col_nr roomType config_params =
    case roomType of
        SquareRoom ->
            square_room_top_left_y config_params row_nr col_nr

        HorizontalRoom ->
            small_height_room_top_left_y config_params row_nr col_nr

        VerticalRoom ->
            small_width_room_top_left_y config_params row_nr col_nr



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


get_room_position_1 : Int -> Int -> RoomType -> ConfigParams -> ( Int, Int )
get_room_position_1 row_nr col_nr roomType config_params =
    let
        width1 =
            toFloat (get_room_width roomType config_params - 2) / 3.0

        height1 =
            toFloat (get_room_height roomType config_params - 2) / 3.0

        pos_x =
            toFloat (get_room_top_left_x row_nr col_nr roomType config_params) + width1 |> Basics.round

        pos_y =
            toFloat (get_room_top_left_y row_nr col_nr roomType config_params) + height1 |> Basics.round
    in
    ( pos_x, pos_y )


get_room_position_nr : Int -> Int -> Int -> RoomType -> ConfigParams -> ( Int, Int )
get_room_position_nr row_nr col_nr pos_nr roomType config_params =
    let
        ( w_factor, h_factor ) =
            get_width_height_factors pos_nr

        add_width =
            toFloat (get_room_width roomType config_params - 2) * w_factor / 3.0

        add_height =
            toFloat (get_room_height roomType config_params - 2) * h_factor / 3.0

        pos_x =
            toFloat (get_room_top_left_x row_nr col_nr roomType config_params) + add_width |> Basics.round

        pos_y =
            toFloat (get_room_top_left_y row_nr col_nr roomType config_params) + add_height |> Basics.round
    in
    ( pos_x, pos_y )


getItemCoordsAndItemInfo : ConfigParams -> ItemCreationInfo -> ( ( Int, Int ), ItemCreationInfo )
getItemCoordsAndItemInfo config_params itemInfo =
    let
        ( x_coord, y_coord ) =
            get_room_position_nr itemInfo.room_row_nr itemInfo.room_col_nr itemInfo.position_in_room itemInfo.room_type config_params
                |> (\( x, y ) -> ( x + (Maybe.map Tuple.first itemInfo.mbLocationShift |> Maybe.withDefault 0), y + (Maybe.map Tuple.second itemInfo.mbLocationShift |> Maybe.withDefault 0) ))
    in
    ( ( x_coord, y_coord ), itemInfo )


setItemsInGrid : ConfigParams -> Dict ItemId ItemCreationInfo -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
setItemsInGrid config_params dItemsToCreate grid =
    let
        lcoordsAndInfo =
            dItemsToCreate
                |> Dict.values
                |> List.map (getItemCoordsAndItemInfo config_params)

        tileItem i_c_info =
            GameModel.Floor (createItemFloorInfo i_c_info)

        setTileFloorItem xcoord ycoord i_creation_info_ grid_ =
            Grid.set (Grid.Coordinate xcoord ycoord) (tileItem i_creation_info_) grid_
    in
    List.foldl (\( ( x_coord, y_coord ), i_creation_info ) gridacc -> setTileFloorItem x_coord y_coord i_creation_info gridacc) grid lcoordsAndInfo


createItemFloorInfo : ItemCreationInfo -> GameModel.FloorInfo
createItemFloorInfo i_c_info =
    GameModel.FloorInfo (Just i_c_info.item) Nothing True True False GameModel.Unexplored ""


setTeleportersInGrid : ConfigParams -> Dict TeleporterId TeleporterInfo -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
setTeleportersInGrid config_params teleporterDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values teleporterDict_
                |> List.map (getTeleportersCoordsAndTeleportersInfo config_params)

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


createTeleporterInfo : Int -> Int -> TeleporterType -> Int -> Int -> RoomType -> String -> Int -> ( Int, Int ) -> GameModel.TeleporterInfo
createTeleporterInfo teleporterId floorId teleportertype row_nr col_nr room_type wall targetId shift =
    GameModel.TeleporterInfo teleporterId floorId teleportertype row_nr col_nr room_type wall targetId shift False GameModel.Unexplored


createTeleporterWallInfo : GameModel.WallInfo -> TeleporterInfo -> GameModel.WallInfo
createTeleporterWallInfo winfo tel_info =
    { winfo | mbTeleporterObject = Just tel_info }


getTeleportersByFloorId : Int -> Dict TeleporterId TeleporterInfo -> Dict TeleporterId TeleporterInfo
getTeleportersByFloorId floorId dTeleporters =
    Dict.filter (\k v -> v.floor_id == floorId) dTeleporters


getTeleportersCoordsAndTeleportersInfo : ConfigParams -> TeleporterInfo -> ( ( Int, Int ), TeleporterInfo )
getTeleportersCoordsAndTeleportersInfo config_params teleporterInfo =
    let
        ( x_coord, y_coord ) =
            get_xy_coords_from_room_row_col teleporterInfo.room_row_nr teleporterInfo.room_col_nr teleporterInfo.room_type teleporterInfo.position_in_room config_params

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


get_xy_coords_from_room_row_col : Int -> Int -> RoomType -> String -> ConfigParams -> ( Int, Int )
get_xy_coords_from_room_row_col row_nr col_nr rtype pos_in_room config_params =
    let
        top_left_x =
            get_room_top_left_x row_nr col_nr rtype config_params

        top_left_y =
            get_room_top_left_y row_nr col_nr rtype config_params

        room_width =
            get_room_width rtype config_params

        room_height =
            get_room_height rtype config_params

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


defaultVerticalYellowDoorOptions : GameModel.DoorWallOptions
defaultVerticalYellowDoorOptions =
    { left = GameModel.NoDoorNoWall
    , top = GameModel.UseDoor (GameModel.defaultYellowDoorInfo GameModel.DoorToDown)
    , right = GameModel.NoDoorNoWall
    , bottom = GameModel.UseDoor (GameModel.defaultYellowDoorInfo GameModel.DoorToUp)
    }


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


getLandingTargetsByFloorId : Int -> Dict TargetId LandingTargetInfo -> Dict TargetId LandingTargetInfo
getLandingTargetsByFloorId floorId dlandingTargets =
    Dict.filter (\k v -> v.floor_id == floorId) dlandingTargets


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


getHolesByFloorId : Int -> Dict HoleId HoleInfo -> Dict HoleId HoleInfo
getHolesByFloorId floorId dHoles =
    Dict.filter (\k v -> v.floorId == floorId) dHoles


createHoleInfo : Int -> Int -> Int -> Int -> Int -> GameModel.HoleInfo
createHoleInfo holeId floorId x y targetId =
    HoleInfo holeId floorId x y targetId False GameModel.Unexplored
