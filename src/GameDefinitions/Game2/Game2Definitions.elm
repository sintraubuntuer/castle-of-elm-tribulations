module GameDefinitions.Game2.Game2Definitions exposing (initialModelFunc)

import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
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
import Grid
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
    Beings.playerCreationFunc elem "You"


initialFightingCharacter : FightingCharacterId -> String -> Int -> FightingCharacter
initialFightingCharacter fcharId species floorId =
    let
        elem =
            "e" ++ String.fromInt fcharId
    in
    Beings.fightingCharacterCreationFunc elem fcharId ("fightingChar" ++ String.fromInt fcharId) species floorId


otherCharacterFunc =
    Beings.otherCharacterCreationFunc 1 "otherYou" lastFloor_id


customGameCompletionFunc : Int -> Grid.Coordinate -> Bool
customGameCompletionFunc floorid coords =
    floorid == lastFloor_id && coords.x < 12 && coords.y >= 5


initialModelFunc : List Int -> Maybe String -> ( GameModel.Model, Bool, Bool )
initialModelFunc lrandints imgBaseDir_ =
    let
        player =
            initialPlayer

        fightingCharacter1 =
            initialFightingCharacter 1 "ghost" caverns_floor_id

        fightingCharacter2 =
            initialFightingCharacter 2 "snake" caverns_floor_id

        fightingCharacter3 =
            initialFightingCharacter 3 "bat" basement_floor_id

        fightingCharacter4 =
            initialFightingCharacter 4 "slime" basement_floor_id

        fightingCharacter5 =
            initialFightingCharacter 5 "small_worm" groundFloor_id

        fightingCharacter6 =
            initialFightingCharacter 6 "pumpking" groundFloor_id

        fightingCharacter7 =
            initialFightingCharacter 7 "ghost" firstFloor_id

        fightingCharacter8 =
            initialFightingCharacter 8 "pumpking" firstFloor_id

        fightingCharacter9 =
            initialFightingCharacter 9 "ghost" theAttic_id

        fightingCharacter10 =
            initialFightingCharacter 10 "bat" theAttic_id

        levers =
            Dict.empty

        ( storeDictWithPlacedPapers, lrands ) =
            place_three_pieces_of_paper ( dStore, lrandints )
                |> place_all_health_food_items 5

        otherCharacter =
            otherCharacterFunc

        randomlyPositionPlayer =
            False

        createRandomMap =
            False
    in
    ( { player = { player | location = { x = 67, y = 36 } }
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
      , level = Dict.get groundFloor_id storeDictWithPlacedPapers |> Maybe.map .level |> Maybe.withDefault GroundFloor.gridGroundFloor -- Grid.Grid Tile
      , explored = setAllAsUnexplored GroundFloor.gridGroundFloor -- Grid.Grid Visibility
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
      , floorDict = storeDictWithPlacedPapers
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
            , total_width = get_total_width config_params 6 -- basement has 6 room columns
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
            , total_width = get_total_width config_params 6 -- firstFloor has 6 room columns
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

            else if not (inList nr lexisting) then
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


place_one_item_in_random_coords : Int -> Item.Item -> ( Dict Int GameModel.FloorStore, List Int ) -> ( Dict Int GameModel.FloorStore, List Int )
place_one_item_in_random_coords floorId newItem ( storedict, lrandints ) =
    let
        auxFuncCheckIfEmptyAndPlaceOrRepeat floorRec floorid ( storeDict, lrands, try_nr ) =
            if try_nr > 100 then
                ( storeDict, lrands )

            else
                let
                    lcoords =
                        Grid.toCoordinates floorRec.level

                    ( index_nr, lrem_rands ) =
                        ( getRandIntNr_ZeroTo (List.length lcoords - 1) lrands, List.drop 1 lrands )

                    thelevel =
                        floorRec.level

                    mbcoords =
                        lcoords |> List.drop index_nr |> List.head

                    mbNewLevel =
                        case mbcoords of
                            Nothing ->
                                Nothing

                            Just coords ->
                                thelevel
                                    |> Grid.get coords
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
                                                                    Just (Grid.set coords tileWithItem thelevel)

                                                                Just it ->
                                                                    Nothing

                                                        _ ->
                                                            Nothing
                                       )

                    mbNewFloor =
                        case mbNewLevel of
                            Nothing ->
                                Nothing

                            Just newLevel ->
                                Just { floorRec | level = newLevel }
                in
                case mbNewFloor of
                    Nothing ->
                        auxFuncCheckIfEmptyAndPlaceOrRepeat floorRec floorid ( storeDict, lrem_rands, try_nr + 1 )

                    Just newFloor ->
                        ( Dict.update floorid (\_ -> Just newFloor) storeDict, lrem_rands )

        floorInfo =
            Dict.get floorId storedict

        ( newStore, remainingRands ) =
            case floorInfo of
                Just afloorinfo ->
                    auxFuncCheckIfEmptyAndPlaceOrRepeat afloorinfo floorId ( storedict, lrandints, 1 )

                Nothing ->
                    ( storedict, lrandints )
    in
    ( newStore, remainingRands )


place_three_pieces_of_paper : ( Dict Int GameModel.FloorStore, List Int ) -> ( Dict Int GameModel.FloorStore, List Int )
place_three_pieces_of_paper ( storedict, lrands ) =
    let
        newPaper pidx =
            Item.Paper (Item.PaperInfo pidx "" "" "")

        ( lfloornr_paperItemTuples, lremainingrands ) =
            generate_three_random_nrs_between_zero_and_four lrands
                |> (\( lx, ly ) -> ( List.indexedMap (\i fnr -> ( fnr, newPaper (i + 1) )) lx, ly ))
    in
    List.foldl (\( fid, paperitem ) ( storeacc, lrand ) -> place_one_item_in_random_coords fid paperitem ( storeacc, lrand )) ( storedict, lremainingrands ) lfloornr_paperItemTuples


place_health_food_items_in_random_coords : Int -> Int -> ( Dict Int GameModel.FloorStore, List Int ) -> ( Dict Int GameModel.FloorStore, List Int )
place_health_food_items_in_random_coords floorid nrItems ( storedict, lrands ) =
    let
        newFoodItem =
            Item.Food "bread"

        lfloornr_foodItemTuples =
            --List.range 1 5 |> List.map ( ( floorid , newFoodItem)  )
            List.repeat nrItems ( floorid, newFoodItem )
    in
    List.foldl (\( flid, fooditem ) ( storeacc, lrand ) -> place_one_item_in_random_coords flid fooditem ( storeacc, lrand )) ( storedict, lrands ) lfloornr_foodItemTuples


place_all_health_food_items : Int -> ( Dict Int GameModel.FloorStore, List Int ) -> ( Dict Int GameModel.FloorStore, List Int )
place_all_health_food_items nrItemsPerFloor ( storedict, lrands ) =
    let
        lfloorids =
            [ caverns_floor_id, basement_floor_id, groundFloor_id, firstFloor_id, theAttic_id ]
    in
    List.foldl (\flid ( storeacc, lrand ) -> place_health_food_items_in_random_coords flid nrItemsPerFloor ( storeacc, lrand )) ( storedict, lrands ) lfloorids



-- get a random place from lcoords and make sure its an empty  floor tile . Install piece of paper


getRandIntNr_ZeroTo : Int -> List Int -> Int
getRandIntNr_ZeroTo maxnr lrandints =
    -- lrandints is a list of random integer nrs between 1 and 100 ( inclusive  ) , maxnr also included
    lrandints
        |> List.head
        |> Maybe.map (\x -> toFloat x * toFloat (maxnr + 1) / 100.0 |> Basics.ceiling)
        |> Maybe.withDefault 1
        |> (\x -> x - 1)


inList : a -> List a -> Bool
inList a_val la =
    List.filter (\elem -> elem == a_val) la
        |> List.length
        |> (\x -> x > 0)
