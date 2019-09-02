module GameDefinitions.Common exposing
    ( ConfigParams
    , HoleId
    , HoleInfoWithLocation
    , HorizontalTunnelOrientation(..)
    , ItemCreationInfo
    , ItemId
    , LandingTargetInfo
    , LocationInfo
    , StairsOrientation(..)
    , TargetId
    , TeleporterId
    , TeleporterInfoWithLocation
    , VerticalTunnelOrientation(..)
    , createHoleInfo
    , createHoleInfoWithLocation
    , createTeleporterInfo
    , createTeleporterInfoWithLocation
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
    , grid2dInitializer
    , gridInitializer
    , gridUnexploredInitializer
    , initialFightingCharacter
    , initialModelFunc
    , initialPlayer
    , setAll2dAsUnexplored
    , setAllAsExplored
    , setAllAsUnexplored
    , setHolesInGrid
    , setItemsInGrid
    , setLandingTargetsInGrid
    , setTeleportersInGrid
    )

import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameModel exposing (RoomRectangle, RoomType(..), TunnelRectangle)
import Grid2
import Grid3 as Grid
import Item exposing (Item(..), KeyInfo)
import Thorns.Types
import Tile exposing (HoleInfo, TeleporterInfo, TeleporterType(..), Tile(..), Visibility(..))


type alias HoleId =
    Int


type alias TeleporterId =
    Int


type alias ItemId =
    Int


gridInitializer : Int -> Int -> Int -> ConfigParams -> Grid.Grid Tile
gridInitializer nr_rows nr_cols nr_floors_ config_params =
    Grid.initialize { width = get_total_width config_params nr_cols, height = get_total_height config_params nr_rows, nr_floors = nr_floors_ } NoTileYet


grid2dInitializer : Int -> Int -> ConfigParams -> Grid2.Grid Tile
grid2dInitializer nr_rows nr_cols config_params =
    Grid2.initialize { width = get_total_width config_params nr_cols, height = get_total_height config_params nr_rows } NoTileYet


gridUnexploredInitializer : Int -> Int -> Int -> ConfigParams -> Grid.Grid Visibility
gridUnexploredInitializer nr_rows nr_cols nr_floors_ config_params =
    Grid.initialize { width = get_total_width config_params nr_cols, height = get_total_height config_params nr_rows, nr_floors = nr_floors_ } Unexplored


setAll2dAsUnexplored : Grid2.Grid Tile -> Grid2.Grid Visibility
setAll2dAsUnexplored level =
    let
        grid =
            Grid2.toList level
    in
    --List.map (\row -> List.map (\_ -> Unexplored) row) grid |> Grid2.fromList
    Grid2.map (\_ -> Unexplored) level


setAllAsUnexplored : Grid.Grid Tile -> Grid.Grid Visibility
setAllAsUnexplored level =
    --List.map (\row -> List.map (\_ -> Unexplored) row) grid |> Grid2.fromList
    Grid.map (\_ -> Unexplored) level


setAllAsExplored : Grid.Grid Tile -> Grid.Grid Visibility
setAllAsExplored level =
    --List.map (\row -> List.map (\_ -> Unexplored) row) grid |> Grid2.fromList
    Grid.map (\_ -> Explored) level


initialPlayer : Player
initialPlayer =
    let
        elem =
            "@"
    in
    Beings.playerCreationFunc elem "You" 10 10 0


initialFightingCharacter : FightingCharacterId -> String -> Int -> Int -> Int -> FightingCharacter
initialFightingCharacter fcharId species x_coord y_coord floor_id =
    let
        elem =
            "e" ++ String.fromInt fcharId
    in
    Beings.fightingCharacterCreationFunc elem fcharId ("fightingCharacter" ++ String.fromInt fcharId) species x_coord y_coord floor_id


dimensions : ( Int, Int )
dimensions =
    ( 10, 10 )


initialModelFunc : String -> ( GameModel.Model, Bool )
initialModelFunc imgBaseDir_ =
    let
        player =
            initialPlayer

        fightingCharacter =
            initialFightingCharacter 1 "ghost" 5 5 theFloorId

        fightingCharacter2 =
            initialFightingCharacter 2 "ghost" 10 10 theFloorId

        levers =
            Dict.empty

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            gridInitializer w h 1 cParams

        theFloorId =
            0

        createRandomMap =
            False
    in
    ( { player = player
      , fightingCharacters =
            Dict.fromList
                [ ( 1, fightingCharacter )
                , ( 2, fightingCharacter2 )
                ]
      , otherCharacters = Dict.empty
      , level = firstMap -- Grid2.Grid Tile
      , explored = setAllAsUnexplored firstMap -- Grid2.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing (Just imgBaseDir_)
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , viewport_topleft_x = 3 -- Int
      , viewport_topleft_y = 3 --Int
      , window_width = 10
      , window_height = 10
      , total_width = Tuple.first dimensions
      , total_height = Tuple.second dimensions
      , currentDisplay = GameModel.DisplayRegularGame
      , displayStatsOverlay = False
      , showBlood = True
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = Dict.empty
      , currentFloorId = theFloorId
      , gameCompletionFunc = \fid coords -> False
      , leverModelChangerFuncs = Dict.empty
      , imgBaseDir = Just imgBaseDir_
      , started = False
      , debugMode = False
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
    , position_in_room : Int
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


getStairsOnRoom : Int -> Int -> Int -> Int -> Int -> StairsOrientation -> Maybe RoomType -> ( Int, Int ) -> Maybe ( Int, Int ) -> ConfigParams -> Grid2.Grid Tile -> Grid2.Grid Tile
getStairsOnRoom row_nr col_nr stairsId toFloorId toStairsId orientation mbFromRoomType shiftOnDestinationTuple mbShiftLocationTuple config_params grid =
    let
        tunnel_width =
            config_params.vertical_wall_width + 1

        tunnel_height =
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
            Tile.Stairs (Tile.StairsInfo stairsId toFloorId toStairsId shiftOnDestinationTuple False Unexplored)
    in
    Grid2.set (Grid2.Coordinate top_left_x top_left_y) tileStairs grid


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


setItemsInGrid : ConfigParams -> Dict ItemId ItemCreationInfo -> Grid2.Grid Tile -> Grid2.Grid Tile
setItemsInGrid config_params dItemsToCreate grid =
    let
        lcoordsAndInfo =
            dItemsToCreate
                |> Dict.values
                |> List.map (getItemCoordsAndItemInfo config_params)

        tileItem i_c_info =
            Tile.Floor (createItemFloorInfo i_c_info)

        setTileFloorItem xcoord ycoord i_creation_info_ grid_ =
            Grid2.set (Grid2.Coordinate xcoord ycoord) (tileItem i_creation_info_) grid_
    in
    List.foldl (\( ( x_coord, y_coord ), i_creation_info ) gridacc -> setTileFloorItem x_coord y_coord i_creation_info gridacc) grid lcoordsAndInfo


createItemFloorInfo : ItemCreationInfo -> Tile.FloorInfo
createItemFloorInfo i_c_info =
    Tile.FloorInfo (Just i_c_info.item) Nothing True True False Unexplored ""


setTeleportersInGrid : ConfigParams -> Dict TeleporterId TeleporterInfoWithLocation -> Grid2.Grid Tile -> Grid2.Grid Tile
setTeleportersInGrid config_params teleporterDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values teleporterDict_
                |> List.map (getTeleportersCoordsAndTeleportersInfo config_params)

        tryTileTeleporter xcoord ycoord tel_info grid_ =
            let
                c_wall_info =
                    Grid2.get (Grid2.Coordinate xcoord ycoord) grid_
            in
            case c_wall_info of
                Just (Tile.Wall winfo) ->
                    Just (Tile.Wall (createTeleporterWallInfo winfo tel_info))

                _ ->
                    Nothing

        setTileTeleporter xcoord ycoord t_info grid_ =
            case tryTileTeleporter xcoord ycoord t_info grid_ of
                Just atile ->
                    Grid2.set (Grid2.Coordinate xcoord ycoord) atile grid_

                Nothing ->
                    grid_
    in
    List.foldl (\( ( x_coord, y_coord ), tinfo ) gridacc -> setTileTeleporter x_coord y_coord tinfo gridacc) grid lcoordsAndInfo


type alias LocationInfo =
    { room_row_nr : Int
    , room_col_nr : Int
    , room_type : RoomType
    , position_in_room : String
    }


type alias TeleporterInfoWithLocation =
    { teleporterInfo : Tile.TeleporterInfo
    , teleporterLocation : LocationInfo
    }


createTeleporterInfo : Int -> Int -> TeleporterType -> Int -> ( Int, Int ) -> Tile.TeleporterInfo
createTeleporterInfo teleporterId floorId teleportertype targetId shift =
    Tile.TeleporterInfo teleporterId floorId teleportertype targetId shift False Unexplored


createTeleporterInfoWithLocation : Int -> Int -> TeleporterType -> Int -> Int -> RoomType -> String -> Int -> ( Int, Int ) -> TeleporterInfoWithLocation
createTeleporterInfoWithLocation teleporterId floorId teleportertype row_nr col_nr room_type wall targetId shift =
    let
        teleporterInfo =
            Tile.TeleporterInfo teleporterId floorId teleportertype targetId shift False Unexplored

        teleporterLocation =
            LocationInfo row_nr col_nr room_type wall
    in
    TeleporterInfoWithLocation teleporterInfo teleporterLocation


createTeleporterWallInfo : Tile.WallInfo -> TeleporterInfo -> Tile.WallInfo
createTeleporterWallInfo winfo tel_info =
    { winfo | mbTeleporterObject = Just tel_info }


getTeleportersByFloorId : Int -> Dict TeleporterId TeleporterInfoWithLocation -> Dict TeleporterId TeleporterInfoWithLocation
getTeleportersByFloorId floorId dTeleporters =
    Dict.filter (\k v -> v.teleporterInfo.floor_id == floorId) dTeleporters


getTeleportersCoordsAndTeleportersInfo : ConfigParams -> TeleporterInfoWithLocation -> ( ( Int, Int ), TeleporterInfo )
getTeleportersCoordsAndTeleportersInfo config_params teleporterInfLoc =
    let
        ( x_coord, y_coord ) =
            get_xy_coords_from_room_row_col teleporterInfLoc.teleporterLocation.room_row_nr teleporterInfLoc.teleporterLocation.room_col_nr teleporterInfLoc.teleporterLocation.room_type teleporterInfLoc.teleporterLocation.position_in_room config_params

        ( x_c, y_c ) =
            case teleporterInfLoc.teleporterInfo.teleporterType of
                Clock ->
                    if String.toLower teleporterInfLoc.teleporterLocation.position_in_room == "up" || String.toLower teleporterInfLoc.teleporterLocation.position_in_room == "down" then
                        ( x_coord - 2, y_coord )

                    else
                        ( x_coord, y_coord )

                _ ->
                    ( x_coord, y_coord )
    in
    ( ( x_c, y_c ), teleporterInfLoc.teleporterInfo )


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


defaultVerticalYellowDoorOptions : Tile.DoorWallOptions
defaultVerticalYellowDoorOptions =
    { left = Tile.NoDoorNoWall
    , top = Tile.UseDoor (Tile.defaultYellowDoorInfo Tile.DoorToDown)
    , right = Tile.NoDoorNoWall
    , bottom = Tile.UseDoor (Tile.defaultYellowDoorInfo Tile.DoorToUp)
    }


defaultNoDoorOptions : Tile.DoorWallOptions
defaultNoDoorOptions =
    { left = Tile.NoDoorNoWall
    , top = Tile.NoDoorNoWall
    , right = Tile.NoDoorNoWall
    , bottom = Tile.NoDoorNoWall
    }


defaultHorizontalOpenDoorOptions : Tile.DoorWallOptions
defaultHorizontalOpenDoorOptions =
    { left = Tile.UseDoor (Tile.defaultOpenDoorInfo Tile.DoorToTheRight)
    , top = Tile.NoDoorNoWall
    , right = Tile.UseDoor (Tile.defaultOpenDoorInfo Tile.DoorToTheLeft)
    , bottom = Tile.NoDoorNoWall
    }


defaultVerticalOpenDoorOptions : Tile.DoorWallOptions
defaultVerticalOpenDoorOptions =
    { left = Tile.NoDoorNoWall
    , top = Tile.UseDoor (Tile.defaultOpenDoorInfo Tile.DoorToDown)
    , right = Tile.NoDoorNoWall
    , bottom = Tile.UseDoor (Tile.defaultOpenDoorInfo Tile.DoorToUp)
    }


defaultHorizontalBlueDoorOptions : Tile.DoorWallOptions
defaultHorizontalBlueDoorOptions =
    { left = Tile.UseDoor (Tile.defaultBlueDoorInfo Tile.DoorToTheRight)
    , top = Tile.NoDoorNoWall
    , right = Tile.UseDoor (Tile.defaultBlueDoorInfo Tile.DoorToTheLeft)
    , bottom = Tile.NoDoorNoWall
    }


defaultVerticalBlueDoorOptions : Tile.DoorWallOptions
defaultVerticalBlueDoorOptions =
    { left = Tile.NoDoorNoWall
    , top = Tile.UseDoor (Tile.defaultBlueDoorInfo Tile.DoorToDown)
    , right = Tile.NoDoorNoWall
    , bottom = Tile.UseDoor (Tile.defaultBlueDoorInfo Tile.DoorToUp)
    }


defaultHorizontalRedDoorOptions : Tile.DoorWallOptions
defaultHorizontalRedDoorOptions =
    { left = Tile.UseDoor (Tile.defaultRedDoorInfo Tile.DoorToTheRight)
    , top = Tile.NoDoorNoWall
    , right = Tile.UseDoor (Tile.defaultRedDoorInfo Tile.DoorToTheLeft)
    , bottom = Tile.NoDoorNoWall
    }


defaultVerticalRedDoorOptions : Tile.DoorWallOptions
defaultVerticalRedDoorOptions =
    { left = Tile.NoDoorNoWall
    , top = Tile.UseDoor (Tile.defaultRedDoorInfo Tile.DoorToDown)
    , right = Tile.NoDoorNoWall
    , bottom = Tile.UseDoor (Tile.defaultRedDoorInfo Tile.DoorToUp)
    }


defaultHorizontalGreenDoorOptions : Tile.DoorWallOptions
defaultHorizontalGreenDoorOptions =
    { left = Tile.UseDoor (Tile.defaultGreenDoorInfo Tile.DoorToTheRight)
    , top = Tile.NoDoorNoWall
    , right = Tile.UseDoor (Tile.defaultGreenDoorInfo Tile.DoorToTheLeft)
    , bottom = Tile.NoDoorNoWall
    }


defaultVerticalGreenDoorOptions : Tile.DoorWallOptions
defaultVerticalGreenDoorOptions =
    { left = Tile.NoDoorNoWall
    , top = Tile.UseDoor (Tile.defaultGreenDoorInfo Tile.DoorToDown)
    , right = Tile.NoDoorNoWall
    , bottom = Tile.UseDoor (Tile.defaultGreenDoorInfo Tile.DoorToUp)
    }


defaultHorizontalYellowDoorOptions : Tile.DoorWallOptions
defaultHorizontalYellowDoorOptions =
    { left = Tile.UseDoor (Tile.defaultYellowDoorInfo Tile.DoorToTheRight)
    , top = Tile.NoDoorNoWall
    , right = Tile.UseDoor (Tile.defaultYellowDoorInfo Tile.DoorToTheLeft)
    , bottom = Tile.NoDoorNoWall
    }


getLandingTargetsCoordsAndTargetInfo : LandingTargetInfo -> ( ( Int, Int ), LandingTargetInfo )
getLandingTargetsCoordsAndTargetInfo targetInfo =
    case targetInfo.mbLocationShift of
        Nothing ->
            ( ( targetInfo.x, targetInfo.y ), targetInfo )

        Just ( x_shift, y_shift ) ->
            ( ( targetInfo.x + x_shift, targetInfo.y + y_shift ), targetInfo )


setLandingTargetsInGrid : Dict TargetId LandingTargetInfo -> Grid2.Grid Tile -> Grid2.Grid Tile
setLandingTargetsInGrid landingTargetsDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values landingTargetsDict_
                |> List.map getLandingTargetsCoordsAndTargetInfo

        tileLandingTarget lt_info =
            Tile.Floor (createLandingTargetFloorInfo lt_info)

        setTileLandingTarget xcoord ycoord h_info grid_ =
            Grid2.set (Grid2.Coordinate xcoord ycoord) (tileLandingTarget h_info) grid_
    in
    List.foldl (\( ( x_coord, y_coord ), hinfo ) gridacc -> setTileLandingTarget x_coord y_coord hinfo gridacc) grid lcoordsAndInfo


createLandingTargetFloorInfo : LandingTargetInfo -> Tile.FloorInfo
createLandingTargetFloorInfo ltinfo =
    Tile.FloorInfo Nothing (Just (Tile.LandingTargetDrawing ltinfo.target_id)) True True False Unexplored ""


getLandingTargetsByFloorId : Int -> Dict TargetId LandingTargetInfo -> Dict TargetId LandingTargetInfo
getLandingTargetsByFloorId floorId dlandingTargets =
    Dict.filter (\k v -> v.floor_id == floorId) dlandingTargets


getHoleCoordsAndHoleInfo : HoleInfoWithLocation -> ( ( Int, Int ), HoleInfo )
getHoleCoordsAndHoleInfo holeInfLoc =
    ( ( holeInfLoc.holeLocation.x, holeInfLoc.holeLocation.y ), holeInfLoc.holeInfo )


setHolesInGrid : Dict HoleId HoleInfoWithLocation -> Grid2.Grid Tile -> Grid2.Grid Tile
setHolesInGrid holesDict_ grid =
    let
        lcoordsAndInfo =
            Dict.values holesDict_
                |> List.map getHoleCoordsAndHoleInfo

        tileHole h_info =
            Hole h_info

        setTileHole xcoord ycoord h_info grid_ =
            Grid2.set (Grid2.Coordinate xcoord ycoord) (tileHole h_info) grid_
    in
    List.foldl (\( ( x_coord, y_coord ), hinfo ) gridacc -> setTileHole x_coord y_coord hinfo gridacc) grid lcoordsAndInfo


getHolesByFloorId : Int -> Dict HoleId HoleInfoWithLocation -> Dict HoleId HoleInfoWithLocation
getHolesByFloorId floorId dHoles =
    Dict.filter (\k v -> v.holeInfo.floorId == floorId) dHoles


type alias HoleInfoWithLocation =
    { holeInfo : HoleInfo
    , holeLocation : Grid2.Coordinate
    }


createHoleInfo : Int -> Int -> Int -> HoleInfo
createHoleInfo holeId floorId targetId =
    HoleInfo holeId floorId targetId False Unexplored


createHoleInfoWithLocation : Int -> Int -> Int -> Int -> Int -> RoomType -> Int -> ConfigParams -> HoleInfoWithLocation
createHoleInfoWithLocation holeId floorId row_nr col_nr pos_nr roomType targetId config_params =
    let
        ( pos_x, pos_y ) =
            get_room_position_nr row_nr col_nr pos_nr roomType config_params
    in
    { holeInfo = HoleInfo holeId floorId targetId False Unexplored
    , holeLocation = Grid2.Coordinate pos_x pos_y
    }
