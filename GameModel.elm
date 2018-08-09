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
    , maxNrEnemyMovesPerTurn : Int -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn : Int
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


player : Element.Element -> String -> Player
player elem pname =
    { location = Grid.Coordinate 10 10
    , avatar = elem
    , name = pname
    , health = 10
    , energy = 10
    , hunger = 10
    , stealth = 20
    , armor = 1
    , protection = 50
    , coordination = 100
    , power = 2
    , initiative = 2 -- this will be altered by generating a random int between 1 and 100
    , placed = False
    }


enemy : Element.Element -> EnemyId -> String -> Enemy
enemy elem enemyid ename =
    { location = Grid.Coordinate 14 4
    , id = enemyid
    , avatar = elem
    , name = ename
    , health = 10
    , stealth = 20
    , armor = 1
    , protection = 50
    , coordination = 100
    , power = 2
    , initiative = 1 -- this will be altered by generating a random int between 1 and 100
    , maxNrEnemyMovesPerTurn = 10 -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn = 0
    , placed = False
    }


location : Int -> Int -> Location
location =
    Grid.Coordinate


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



-}


mbUpdateEnemyInitiativeByMbEnemyId : Int -> Maybe EnemyId -> State -> State
mbUpdateEnemyInitiativeByMbEnemyId intval mbEnemyid state =
    case mbEnemyid of
        Nothing ->
            state

        Just enemyid ->
            let
                newEnemies =
                    Dict.update enemyid (\mbEnemy -> mbEnemy |> Maybe.map (\enemy -> { enemy | initiative = intval })) state.enemies
            in
            { state | enemies = newEnemies }


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
    Grid.neighborhoodCalc 2 state.player.location


visibility : State -> Location -> Visibility
visibility state location =
    Grid.getWithDefault Unexplored location state.explored
