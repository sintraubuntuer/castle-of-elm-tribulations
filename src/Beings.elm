module Beings exposing
    ( CharacterId
    , Direction(..)
    , Enemy
    , EnemyId
    , Inventory
    , Location
    , OPPONENT_INTERACTION_OPTIONS(..)
    , OtherCharacter
    , Player
    , enemyCreationFunc
    , playerCreationFunc
    )

import Dict exposing (Dict)
import Grid
import Item exposing (Item, KeyInfo)


type alias Location =
    Grid.Coordinate


type alias EnemyId =
    Int


type alias CharacterId =
    Int


type alias Player =
    { location : Location
    , textAvatar : String --Element.Element
    , name : String
    , direction : Direction
    , health : Int
    , energy : Int
    , mana : Int
    , inventory : Inventory
    , hunger : Int
    , stealth : Int
    , armor : Int
    , protection : Int
    , coordination : Int
    , power : Int
    , initiative : Int
    , placed : Bool
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Enemy =
    { location : Location
    , floorId : Int
    , id : EnemyId
    , textAvatar : String --Element.Element
    , name : String
    , direction : Direction
    , species : String
    , health : Int
    , indexOfLight : Int
    , mana : Int
    , inventory : Inventory
    , stealth : Int
    , armor : Int
    , protection : Int
    , coordination : Int
    , power : Int
    , initiative : Int
    , attacksUsingGameOfThorns : Bool
    , indexOfLightMax : Int
    , disappearsWhenHealthIsZero : Bool
    , disappearsWhenIndexOfLightMax : Bool
    , maxNrEnemyMovesPerTurn : Int -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn : Int
    , placed : Bool
    }


type alias OtherCharacter =
    { location : Location
    , floorId : Int
    , id : CharacterId
    , textAvatar : String --Element.Element
    , name : String
    , species : String
    , health : Int
    , mana : Int
    , inventory : Inventory
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


type alias Inventory =
    Dict String Item


type OPPONENT_INTERACTION_OPTIONS
    = CHICANE_ATTACK
    | OPPONENT_CHICANE_ATTACK
    | ENLIGHTENMENT_SPELL
    | OPPONENT_ENLIGHTENMENT_SPELL


playerCreationFunc : String -> String -> Player
playerCreationFunc elem pname =
    { location = Grid.Coordinate 10 10
    , textAvatar = elem
    , name = pname
    , direction = Down
    , health = 10
    , energy = 10
    , mana = 100
    , inventory = Dict.empty
    , hunger = 10
    , stealth = 20
    , armor = 1
    , protection = 50
    , coordination = 100
    , power = 2
    , initiative = 2 -- this will be altered by generating a random int between 1 and 100
    , placed = False
    }


enemyCreationFunc : String -> EnemyId -> String -> String -> Int -> Enemy
enemyCreationFunc elem enemyid ename species_ floor_id =
    { location = Grid.Coordinate 14 4
    , floorId = floor_id
    , id = enemyid
    , textAvatar = elem
    , name = ename
    , direction = Down
    , species = species_
    , health = 10
    , indexOfLight = 1
    , mana = 100
    , inventory = Dict.empty
    , stealth = 20
    , armor = 1
    , protection = 50
    , coordination = 100
    , power = 2
    , initiative = 1 -- this will be altered by generating a random int between 1 and 100
    , attacksUsingGameOfThorns = True
    , indexOfLightMax = 11
    , disappearsWhenHealthIsZero = False
    , disappearsWhenIndexOfLightMax = False
    , maxNrEnemyMovesPerTurn = 2 -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn = 0
    , placed = False
    }
