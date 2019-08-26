module GameDefinitions.Game1.Game1Definitions exposing (initialModelFunc)

import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Dict exposing (Dict)
import GameDefinitions.Common exposing (setAllAsUnexplored)
import GameModel
import Grid
import Thorns.Types
import Tile exposing (Tile(..), Visibility(..))



{- }

   setAllAsUnexplored : Grid.Grid Tile -> Grid.Grid Tile.Visibility
   setAllAsUnexplored level =
       let
           grid =
               Grid.toList level
       in
       List.map (\row -> List.map (\_ -> Unexplored) row) grid |> Grid.fromList

-}


initialPlayer : Player
initialPlayer =
    let
        elem =
            "@"
    in
    Beings.playerCreationFunc elem "You"


initialFightingCharacter : FightingCharacterId -> Int -> FightingCharacter
initialFightingCharacter fcharId floorId =
    let
        elem =
            "e" ++ String.fromInt fcharId
    in
    Beings.fightingCharacterCreationFunc elem fcharId ("fightingCharacter" ++ String.fromInt fcharId) "ghost" floorId


dimensions : ( Int, Int )
dimensions =
    ( 80, 60 )


initialModelFunc : ( GameModel.Model, Bool, Bool )
initialModelFunc =
    let
        player =
            initialPlayer

        fightingCharacter =
            initialFightingCharacter 1 theFloorId

        fightingCharacter2 =
            initialFightingCharacter 2 theFloorId

        levers =
            Dict.empty

        w =
            Tuple.first dimensions

        h =
            Tuple.second dimensions

        firstMap =
            -- MapGen.randomCave dimensions
            Grid.initialize { width = w, height = h } Tile.NoTileYet

        roomsInfo =
            Just <| GameModel.RoomsInfo [] 20 12 7

        firstExplored =
            setAllAsUnexplored firstMap

        theFloorId =
            1

        randomlyPositionPlayer =
            True

        createRandomMap =
            True
    in
    -- GameModel.Model
    ( { player = player
      , fightingCharacters =
            Dict.fromList
                [ ( 1, fightingCharacter )
                , ( 2, fightingCharacter2 )
                ]
      , otherCharacters = Dict.empty
      , level = firstMap -- Grid.Grid Tile

      --, levers = levers --Dict LeverId LeverInfo
      , explored = firstExplored -- Grid.Grid Visibility
      , log = [ "you enter the dungeon" ] --List String
      , gameOfThornsModel = Thorns.Types.initialModel player Nothing
      , gameOfThornsModeisOn = False
      , listeningToKeyInput = True
      , pseudoRandomIntsPool = [] -- List Int
      , x_display_anchor = 3 -- Int
      , y_display_anchor = 3 --Int
      , window_width = 15
      , window_height = 12
      , total_width = Tuple.first dimensions
      , total_height = Tuple.second dimensions
      , currentDisplay = GameModel.DisplayRegularGame
      , displayStatsOverlay = False
      , showBlood = True
      , wallPercentage = Nothing -- Maybe Float
      , roomsInfo = roomsInfo --  RoomsInfo
      , floorDict = Dict.empty
      , currentFloorId = theFloorId
      , gameCompletionFunc = \fid coords -> False
      , leverModelChangerFuncs = Dict.empty
      , started = True
      , debugMode = False
      }
    , createRandomMap
    , randomlyPositionPlayer
    )
