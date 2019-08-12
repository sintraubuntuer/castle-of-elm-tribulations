module Beings exposing
    ( CharacterId
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


type alias Enemy =
    { location : Location
    , id : EnemyId
    , textAvatar : String --Element.Element
    , name : String
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


type alias OtherCharacter =
    { location : Location
    , id : CharacterId
    , textAvatar : String --Element.Element
    , name : String
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


enemyCreationFunc : String -> EnemyId -> String -> Enemy
enemyCreationFunc elem enemyid ename =
    { location = Grid.Coordinate 14 4
    , id = enemyid
    , textAvatar = elem
    , name = ename
    , health = 10
    , mana = 100
    , inventory = Dict.empty
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
