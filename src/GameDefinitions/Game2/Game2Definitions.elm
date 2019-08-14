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


initialModelFunc : ( GameModel.Model, Bool, Bool )
initialModelFunc =
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
      , displayInventory = False
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
