module GameModel exposing (..)

--import Generator
--import Generator.Standard
--import Keyboard

import Dict exposing (Dict)
import Element
import Grid
import Text


type alias EnemyId =
    Int


type alias State =
    { player : Player
    , enemies : Dict EnemyId Enemy
    , level : Grid.Grid Tile
    , explored : Grid.Grid Visibility
    , log : List String
    , pseudoRandomIntsPool : List Int

    --, generator : Random
    }


type alias Player =
    { location : Location
    , avatar : Element.Element
    , name : String
    , health : Int
    , energy : Int
    , hunger : Int
    , stealth : Int
    , armor : Int
    , protection : Int
    , coordination : Int
    , power : Int
    , initiative : Int
    , placed : Bool
    }


type alias Enemy =
    { location : Location
    , id : EnemyId
    , avatar : Element.Element
    , name : String
    , health : Int
    , stealth : Int
    , armor : Int
    , protection : Int
    , coordination : Int
    , power : Int
    , initiative : Int
    , placed : Bool
    }


type alias Location =
    Grid.Coordinate


type Tile
    = Floor
    | Wall
    | Door
    | Acid
    | NoTileYet


type Visibility
    = Visible
    | Unexplored
    | Explored


type Input
    = Up
    | Down
    | Left
    | Right
    | Nop



--type Random = Generator.Generator Generator.Standard.Standard


player : Element.Element -> String -> Player
player elem name =
    --let (initiative, gen') = Generator.int32Range (1, 100) gen
    --in
    let
        initiative =
            50

        -- have too rewrite this so its random
    in
    Player (Grid.Coordinate 2 2) elem name 10 10 10 20 1 50 100 2 initiative False


enemy : Element.Element -> EnemyId -> String -> Enemy
enemy elem enemyid name =
    --let (initiative, gen') = Generator.int32Range (1, 100) gen
    --in
    let
        initiative =
            50

        -- have too rewrite this so its random
    in
    Enemy (Grid.Coordinate 14 4) enemyid elem name 10 20 1 50 100 2 initiative False


location : Int -> Int -> Location
location =
    Grid.Coordinate



{-
   handle : KeyCode -> Input
   handle key =
       case key of
           37 ->
               Left

           38 ->
               Up

           39 ->
               Right

           40 ->
               Down

           _ ->
               Nop
-}


validLocation : Location -> State -> Bool
validLocation location state =
    Grid.inGrid location state.level


pathable : Location -> State -> Bool
pathable location state =
    let
        level =
            state.level

        tile =
            Grid.get location level
    in
    case tile of
        Nothing ->
            False

        Just Floor ->
            True

        Just _ ->
            False



{-
      getRandomPathable : State -> ( Location, State )
      getRandomPathable state =
          let
              ( x, gen' ) =
                  Generator.int32Range ( 1, state.level.size.width ) state.generator

              ( y, gen'' ) =
                  Generator.int32Range ( 1, state.level.size.height ) gen'

              locn =
                  location x y

              state' =
                  { state | generator = gen'' }
          in
          case pathable locn state' of
              True ->
                  ( locn, state' )

              False ->
                  getRandomPathable state'




   placeEnemy : Enemy -> State -> State
   placeEnemy a state =
       let
           ( loc, state' ) =
               getRandomPathable state
       in
       { state' | enemies = { a | location = loc, placed = True } :: tail state'.enemies }

-}
{-
   placePlayer : State -> State
   placePlayer state =
       let
           ( loc, state' ) =
               getRandomPathable state

           player' =
               state'.player
       in
       { state' | player = { player' | location = loc, placed = True } }
-}


mbUpdateEnemyLocation : Location -> Maybe Enemy -> Maybe Enemy
mbUpdateEnemyLocation loc mbenemy =
    case mbenemy of
        Nothing ->
            Nothing

        Just en ->
            Just { en | location = loc, placed = True }


placeExistingEnemy : EnemyId -> Location -> Dict EnemyId Enemy -> Dict EnemyId Enemy
placeExistingEnemy enid loc dictacc =
    case Dict.get enid dictacc of
        Nothing ->
            dictacc

        Just enemy ->
            Dict.update enid (\mbenemy -> mbUpdateEnemyLocation loc mbenemy) dictacc


randomlyPlaceExistingEnemies : List ( Location, EnemyId ) -> State -> State
randomlyPlaceExistingEnemies lpairIntIds state =
    let
        dictenemies =
            state.enemies

        newDictEnemies =
            List.foldl (\( loc, enid ) dictacc -> placeExistingEnemy enid loc dictacc) dictenemies lpairIntIds
    in
    { state | enemies = newDictEnemies }


showTile : Tile -> Element.Element
showTile tile =
    let
        c =
            case tile of
                Floor ->
                    " "

                Wall ->
                    "#"

                Door ->
                    "+"

                Acid ->
                    "~"

                NoTileYet ->
                    "n"
    in
    Element.centered << Text.monospace << Text.fromString <| c


visible : State -> List Location
visible state =
    Grid.neighborhood2 state.player.location


visibility : State -> Location -> Visibility
visibility state location =
    Grid.getWithDefault Unexplored location state.explored
