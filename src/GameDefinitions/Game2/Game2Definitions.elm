module GameDefinitions.Game2.Game2Definitions exposing (initialModelFunc)

import Array
import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameDefinitions.Common
    exposing
        ( ItemCreationInfo
        , get_total_height
        , get_total_width
        , gridUnexploredInitializer
        , setAll2dAsUnexplored
        , setAllAsExplored
        , setAllAsUnexplored
        , setItemsInGrid
        )
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
        , lastFloor_id
        , teleporterInfoDict
        , theAttic_id
        )
import GameDefinitions.Game2.FirstFloor as FirstFloor
import GameDefinitions.Game2.GroundFloor as GroundFloor
import GameDefinitions.Game2.LastFloor as LastFloor
import GameDefinitions.Game2.TheAttic as TheAttic
import GameModel
    exposing
        ( RoomRectangle
        , RoomType(..)
        , TunnelRectangle
        )
import Grid2
import Grid3 as Grid
import Item exposing (Item(..), KeyInfo)
import MapGen
import Thorns.Types
import Tile
    exposing
        ( HoleInfo
        , TeleporterInfo
        , TeleporterType(..)
        , Tile(..)
        )


initialPlayer : Player
initialPlayer =
    let
        elem =
            "@"
    in
    Beings.playerCreationFunc elem "You" 10 10 0


initialFightingCharacter : FightingCharacterId -> String -> Int -> Int -> Int -> FightingCharacter
initialFightingCharacter fcharId species x_coord y_coord floorId =
    let
        elem =
            "e" ++ String.fromInt fcharId
    in
    Beings.fightingCharacterCreationFunc elem fcharId ("fightingChar" ++ String.fromInt fcharId) species x_coord y_coord floorId


otherCharacterFunc =
    Beings.otherCharacterCreationFunc 1 "otherYou" 10 10 lastFloor_id


customGameCompletionFunc : Int -> Grid.Coordinate -> Bool
customGameCompletionFunc floorid coords =
    floorid == lastFloor_id && coords.x < 12 && coords.y >= 5


initialModelFunc : List Int -> Maybe String -> ( GameModel.Model, Bool, Bool )
initialModelFunc lrandints imgBaseDir_ =
    let
        player =
            initialPlayer

        fightingCharacter1 =
            initialFightingCharacter 1 "ghost" 5 5 caverns_floor_id

        fightingCharacter2 =
            initialFightingCharacter 2 "snake" 10 10 caverns_floor_id

        fightingCharacter3 =
            initialFightingCharacter 3 "bat" 5 5 basement_floor_id

        fightingCharacter4 =
            initialFightingCharacter 4 "slime" 10 10 basement_floor_id

        fightingCharacter5 =
            initialFightingCharacter 5 "small_worm" 5 5 groundFloor_id

        fightingCharacter6 =
            initialFightingCharacter 6 "pumpking" 10 10 groundFloor_id

        fightingCharacter7 =
            initialFightingCharacter 7 "ghost" 5 5 firstFloor_id

        fightingCharacter8 =
            initialFightingCharacter 8 "pumpking" 10 10 firstFloor_id

        fightingCharacter9 =
            initialFightingCharacter 9 "ghost" 5 5 theAttic_id

        fightingCharacter10 =
            initialFightingCharacter 10 "bat" 10 10 theAttic_id

        levers =
            Dict.empty

        ( gridWithPlacedPapersAndHealthItems, lrands ) =
            place_three_pieces_of_paper ( grid3, lrandints )
                |> place_all_health_food_items 5

        --|> (\( x, y ) -> ( Grid.map (\t -> Tile.setTileVisibility Tile.Visible t) x, y ))
        otherCharacter =
            otherCharacterFunc

        randomlyPositionPlayer =
            False

        createRandomMap =
            False
    in
    ( { player = { player | location = { x = 67, y = 36, z = 2 } }
      , fightingCharacters =
            Dict.fromList
                [ ( fightingCharacter1.id, fightingCharacter1 )
                , ( fightingCharacter2.id, fightingCharacter2 )
                , ( fightingCharacter3.id, fightingCharacter3 )
                , ( fightingCharacter4.id, fightingCharacter4 )
                , ( fightingCharacter5.id, fightingCharacter5 )
                , ( fightingCharacter6.id, fightingCharacter6 )
                , ( fightingCharacter7.id, fightingCharacter7 )
                , ( fightingCharacter8.id, fightingCharacter8 )
                , ( fightingCharacter9.id, fightingCharacter9 )
                , ( fightingCharacter10.id, fightingCharacter10 )
                ]
      , otherCharacters =
            Dict.fromList
                [ ( 1, otherCharacter )
                ]
      , level = gridWithPlacedPapersAndHealthItems -- Dict.get groundFloor_id storeDictWithPlacedPapers |> Maybe.map .level |> Maybe.withDefault GroundFloor.gridGroundFloor -- Grid.Grid Tile
      , explored = gridUnexploredInitializer 10 10 5 config_params -- setAllAsExplored grid3 -- GroundFloor.gridGroundFloor -- Grid.Grid Visibility
      , log = [ "you enter the dungeons Ground Floor " ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing imgBaseDir_
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , viewport_topleft_x = 3 -- Int , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , viewport_topleft_y = 3 --Int   , this value doesn't really matter because after the player is randomly placed this value is readjusted
      , window_width = common_window_width
      , window_height = common_window_height
      , total_width = get_total_width config_params 7
      , total_height = get_total_height config_params 9
      , currentDisplay = GameModel.DisplayRegularGame
      , displayStatsOverlay = False
      , showBlood = True
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = dStore --storeDictWithPlacedPapers
      , currentFloorId = groundFloor_id
      , gameCompletionFunc = customGameCompletionFunc
      , leverModelChangerFuncs = LastFloor.leverModelChangerFuncs
      , imgBaseDir = imgBaseDir_
      , started = True
      , debugMode = False
      }
    , createRandomMap
    , randomlyPositionPlayer
    )


common_window_width : Int
common_window_width =
    12


common_window_height : Int
common_window_height =
    12


grid3 : Grid.Grid Tile
grid3 =
    { grid =
        [ Caverns.gridCaverns.grid
        , Basement.gridBasement.grid
        , GroundFloor.gridGroundFloor.grid
        , FirstFloor.gridFirstFloor.grid
        , TheAttic.gridTheAttic.grid
        , LastFloor.gridLastFloor.grid
        ]
            |> Array.fromList
    }


dStore : Dict Int GameModel.FloorStore
dStore =
    Dict.fromList
        [ ( 0
          , { window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 11 -- caverns has 11 room columns
            , total_height = get_total_height config_params 7 -- 7 room rows
            }
          )
        , ( 1
          , { window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 6 -- basement has 6 room columns
            , total_height = get_total_height config_params 7 -- 7 room rows
            }
          )
        , ( 2
          , { window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 7 -- groundFloor has 7room columns
            , total_height = get_total_height config_params 9 -- 9 room rows
            }
          )
        , ( 3
          , { window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 6 -- firstFloor has 6 room columns
            , total_height = get_total_height config_params 7 -- 7 room rows
            }
          )
        , ( 4
          , { window_width = common_window_width
            , window_height = common_window_height
            , total_width = get_total_width config_params 4 -- theAttic has 4 room columns
            , total_height = get_total_height config_params 4 -- 4 room rows
            }
          )
        , ( 5
          , { window_width = 18
            , window_height = 18
            , total_width = get_total_width config_params 23 -- LastFloor has 23  columns
            , total_height = get_total_height config_params 17 -- 17  rows
            }
          )
        ]


generate_three_random_nrs_between_zero_and_four : List Int -> ( List Int, List Int )
generate_three_random_nrs_between_zero_and_four lrandints =
    let
        getRand maxnr ( lexisting, lrand_ints, try_nr ) =
            let
                nr =
                    getRandIntNr_ZeroTo maxnr lrand_ints
            in
            if List.length lexisting >= maxnr || try_nr > 100 then
                ( lexisting, lrand_ints )

            else if not (List.member nr lexisting) then
                ( nr :: lexisting, List.drop 1 lrand_ints )

            else
                getRand maxnr ( lexisting, List.drop 1 lrand_ints, try_nr + 1 )

        ( l1, lrands1 ) =
            getRand 4 ( [], lrandints, 1 )

        ( l2, lrands2 ) =
            getRand 4 ( l1, lrands1, 1 )

        ( l3, lrands3 ) =
            getRand 4 ( l2, lrands2, 1 )
    in
    ( l3, lrands3 )


place_one_item_in_random_coords : Int -> Item.Item -> ( Grid.Grid Tile, List Int ) -> ( Grid.Grid Tile, List Int )
place_one_item_in_random_coords floorId newItem ( grid_, lrandints ) =
    let
        auxFuncCheckIfEmptyAndPlaceOrRepeat floorid ( grid, lrands, try_nr ) =
            if try_nr > 100 then
                ( grid, lrands )

            else
                let
                    lcoords =
                        --Grid.toCoordinates grid
                        --    |> List.filter (\coords -> coords.z == floorId)
                        Grid.getFloorGrid floorid grid
                            |> Maybe.map Grid2.toCoordinates
                            |> Maybe.withDefault []

                    ( index_nr, lrem_rands ) =
                        ( getRandIntNr_ZeroTo (List.length lcoords - 1) lrands, List.drop 1 lrands )

                    --thelevel =
                    --    floorRec.level
                    mbcoords =
                        lcoords |> List.drop index_nr |> List.head |> Maybe.map (\c -> Grid.Coordinate c.x c.y floorid)

                    mbNewGrid =
                        case mbcoords of
                            Nothing ->
                                Nothing

                            Just coords ->
                                Grid.get coords grid
                                    |> (\mbtile ->
                                            case mbtile of
                                                Nothing ->
                                                    Nothing

                                                Just atile ->
                                                    case atile of
                                                        Floor finfo ->
                                                            let
                                                                tileWithItem =
                                                                    Floor { finfo | item = Just newItem }
                                                            in
                                                            case finfo.item of
                                                                Nothing ->
                                                                    Just (Grid.set coords tileWithItem grid)

                                                                Just it ->
                                                                    Nothing

                                                        _ ->
                                                            Nothing
                                       )
                in
                case mbNewGrid of
                    Nothing ->
                        auxFuncCheckIfEmptyAndPlaceOrRepeat floorid ( grid, lrem_rands, try_nr + 1 )

                    Just newGrid ->
                        ( newGrid, lrem_rands )

        ( updatedGrid, remainingRands ) =
            auxFuncCheckIfEmptyAndPlaceOrRepeat floorId ( grid_, lrandints, 1 )
    in
    ( updatedGrid, remainingRands )


place_three_pieces_of_paper : ( Grid.Grid Tile, List Int ) -> ( Grid.Grid Tile, List Int )
place_three_pieces_of_paper ( grid, lrands ) =
    let
        newPaper pidx =
            Item.Paper (Item.PaperInfo pidx "" "" "")

        ( lfloornr_paperItemTuples, lremainingrands ) =
            generate_three_random_nrs_between_zero_and_four lrands
                |> (\( lx, ly ) -> ( List.indexedMap (\i fnr -> ( fnr, newPaper (i + 1) )) lx, ly ))
    in
    List.foldl (\( fid, paperitem ) ( gridacc, lrand ) -> place_one_item_in_random_coords fid paperitem ( gridacc, lrand )) ( grid, lremainingrands ) lfloornr_paperItemTuples


place_health_food_items_in_random_coords : Int -> Int -> ( Grid.Grid Tile, List Int ) -> ( Grid.Grid Tile, List Int )
place_health_food_items_in_random_coords floorid nrItems ( grid, lrands ) =
    let
        newFoodItem =
            Item.Food "bread"

        lfloornr_foodItemTuples =
            --List.range 1 5 |> List.map ( ( floorid , newFoodItem)  )
            List.repeat nrItems ( floorid, newFoodItem )
    in
    List.foldl (\( flid, fooditem ) ( gridacc, lrand ) -> place_one_item_in_random_coords flid fooditem ( gridacc, lrand )) ( grid, lrands ) lfloornr_foodItemTuples


place_all_health_food_items : Int -> ( Grid.Grid Tile, List Int ) -> ( Grid.Grid Tile, List Int )
place_all_health_food_items nrItemsPerFloor ( grid, lrands ) =
    let
        lfloorids =
            [ caverns_floor_id, basement_floor_id, groundFloor_id, firstFloor_id, theAttic_id ]
    in
    List.foldl (\flid ( gridacc, lrand ) -> place_health_food_items_in_random_coords flid nrItemsPerFloor ( gridacc, lrand )) ( grid, lrands ) lfloorids



-- get a random place from lcoords and make sure its an empty  floor tile . Install piece of paper


getRandIntNr_ZeroTo : Int -> List Int -> Int
getRandIntNr_ZeroTo maxnr lrandints =
    -- lrandints is a list of random integer nrs between 1 and 100 ( inclusive  ) , maxnr also included
    lrandints
        |> List.head
        |> Maybe.map (\x -> toFloat x * toFloat (maxnr + 1) / 100.0 |> Basics.ceiling)
        |> Maybe.withDefault 1
        |> (\x -> x - 1)
