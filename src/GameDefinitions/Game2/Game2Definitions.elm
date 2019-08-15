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


initialEnemy : EnemyId -> String -> Int -> Enemy
initialEnemy enemyid species floorId =
    let
        elem =
            "e" ++ String.fromInt enemyid

        --|> Text.fromString
        --|> Text.monospace
        --|> Text.color white
        --|> centered
    in
    Beings.enemyCreationFunc elem enemyid ("enemy" ++ String.fromInt enemyid) species floorId



--( 92, 60 )


initialModelFunc : List Int -> ( GameModel.Model, Bool, Bool )
initialModelFunc lrandints =
    let
        player =
            initialPlayer

        enemy1 =
            initialEnemy 1 "ghost" caverns_floor_id

        enemy2 =
            initialEnemy 2 "snake" caverns_floor_id

        enemy3 =
            initialEnemy 3 "bat" basement_floor_id

        enemy4 =
            initialEnemy 4 "slime" basement_floor_id

        enemy5 =
            initialEnemy 5 "small_worm" groundFloor_id

        enemy6 =
            initialEnemy 6 "pumpking" groundFloor_id

        levers =
            Dict.empty

        ( storeDictWithPlacedPapers, lrands ) =
            place_three_pieces_of_paper ( dStore, lrandints )

        _ =
            Debug.log " size of lrands after placing pieces of paper is  " (List.length lrands)

        randomlyPositionPlayer =
            False

        createRandomMap =
            False
    in
    -- GameModel.Model
    ( { player = { player | location = { x = 60, y = 36 } }
      , enemies =
            Dict.fromList
                [ ( enemy1.id, enemy1 )
                , ( enemy2.id, enemy2 )
                , ( enemy3.id, enemy3 )
                , ( enemy4.id, enemy4 )
                , ( enemy5.id, enemy5 )
                , ( enemy6.id, enemy6 )
                ]
      , otherCharacters = Dict.empty
      , level = Dict.get groundFloor_id storeDictWithPlacedPapers |> Maybe.map .level |> Maybe.withDefault GroundFloor.gridGroundFloor -- Grid.Grid Tile

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
      , displayInventory = False
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = Nothing --  RoomsInfo
      , floorDict = storeDictWithPlacedPapers
      , currentFloorId = 2
      , started = True
      }
    , createRandomMap
    , randomlyPositionPlayer
    )


common_window_width : Int
common_window_width =
    14


common_window_height : Int
common_window_height =
    14


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
            , total_width = get_total_width config_params 23 -- theAttic has 17 room columns
            , total_height = get_total_height config_params 17 -- 17 room rows
            }
          )
        ]


generate_three_random_nrs_between_zero_and_four : List Int -> ( List Int, List Int )
generate_three_random_nrs_between_zero_and_four lrandints =
    let
        getRand maxnr ( lexisting, lrand_ints, try_nr ) =
            let
                _ =
                    Debug.log "getRand in generate_three_random_nrs_between_zero_and_four called. try_nr =  " try_nr

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


place_one_piece_of_paper : Int -> Int -> ( Dict Int GameModel.FloorStore, List Int ) -> ( Dict Int GameModel.FloorStore, List Int )
place_one_piece_of_paper floorId paperid ( storedict, lrandints ) =
    let
        --grid1 =
        --    Grid.get floorId
        auxFuncCheckIfEmptyAndPlaceOrRepeat floorRec floorid ( storeDict, lrands, try_nr ) =
            if try_nr > 100 then
                ( storeDict, lrands )

            else
                let
                    _ =
                        Debug.log "auxFuncCheckIfEmptyAndPlaceOrRepeat called for floorId " floorid

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
                                                    --tryAgainWithNewCoords
                                                    --auxFuncCheckIfEmptyAndPlaceOrRepeat floorRec ( storeDict, lrem_rands, try_nr + 1 )
                                                    Nothing

                                                Just atile ->
                                                    case atile of
                                                        GameModel.Floor finfo ->
                                                            let
                                                                newItem =
                                                                    Item.Paper (Item.PaperInfo paperid "" "" "")

                                                                tileWithItem =
                                                                    GameModel.Floor { finfo | item = Just newItem }
                                                            in
                                                            case finfo.item of
                                                                Nothing ->
                                                                    let
                                                                        _ =
                                                                            Debug.log ("going to place one of the three items in floorId " ++ String.fromInt floorid ++ " and coords : ") coords
                                                                    in
                                                                    Just (Grid.set coords tileWithItem thelevel)

                                                                Just it ->
                                                                    Nothing

                                                        _ ->
                                                            --tryAgainWithNewCoords
                                                            --auxFuncCheckIfEmptyAndPlaceOrRepeat floorRec ( storeDict, lrem_rands, try_nr + 1 )
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

        -- or  auxFuncCheckIfEmptyAndPlaceOrRepeat floorid coords (storeDict , try_nr + 1 )
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
        ( lfloornr_paperIdTuples, lremainingrands ) =
            generate_three_random_nrs_between_zero_and_four lrands
                |> (\( lx, ly ) -> ( List.indexedMap (\i fnr -> ( fnr, i + 1 )) lx, ly ))

        _ =
            Debug.log "going to place three pieces of paper on floors " lfloornr_paperIdTuples
    in
    List.foldl (\( fid, paperid ) ( storeacc, lrand ) -> place_one_piece_of_paper fid paperid ( storeacc, lrand )) ( storedict, lremainingrands ) lfloornr_paperIdTuples



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
