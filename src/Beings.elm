module Beings exposing
    ( CharacterId
    , Direction(..)
    , Enemy
    , EnemyId
    , EnlightenmentSpellEffect(..)
    , Inventory
    , Location
    , OPPONENT_INTERACTION_OPTIONS(..)
    , OtherCharacter
    , Player
    , enemyCreationFunc
    , otherCharacterCreationFunc
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


type EnlightenmentSpellEffect
    = DecreaseHealth
    | IncreaseIndexOfLight
    | DecreaseIndexOfLight


type alias Player =
    { location : Location
    , textAvatar : String --Element.Element
    , name : String
    , direction : Direction
    , health : Int
    , indexOfLight : Int
    , energy : Int
    , mana : Int
    , enlSpellEffect : EnlightenmentSpellEffect
    , inventory : Inventory
    , hunger : Int
    , stealth : Int
    , armor : Int
    , protection : Int
    , coordination : Int
    , power : Int
    , initiative : Int
    , indexOfLightMax : Int
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
    , enlSpellEffect : EnlightenmentSpellEffect
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
    , playerCanWalkOverIfDead : Bool
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
    , direction : Direction
    , species : String
    , health : Int
    , indexOfLight : Int
    , mana : Int
    , enlSpellEffect : EnlightenmentSpellEffect
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
    , playerCanWalkOverIfDead : Bool
    , disappearsWhenIndexOfLightMax : Bool
    , maxNrEnemyMovesPerTurn : Int -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn : Int
    , placed : Bool
    }


otherCharacterCreationFunc : Int -> String -> Int -> OtherCharacter
otherCharacterCreationFunc id_ ename floor_id_ =
    { location = Grid.Coordinate 9 9
    , floorId = floor_id_
    , id = id_
    , textAvatar = "" --Element.Element
    , name = ename
    , direction = Down
    , species = "otherYou"
    , health = 30
    , indexOfLight = 8
    , mana = 100
    , enlSpellEffect = IncreaseIndexOfLight
    , inventory = Dict.empty
    , stealth = 20
    , armor = 1
    , protection = 5
    , coordination = 20
    , power = 10
    , initiative = 100
    , attacksUsingGameOfThorns = False
    , indexOfLightMax = 30
    , disappearsWhenHealthIsZero = True
    , playerCanWalkOverIfDead = True
    , disappearsWhenIndexOfLightMax = False
    , maxNrEnemyMovesPerTurn = 1 -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn = 0
    , placed = True
    }


type alias Inventory =
    Dict String Item


type OPPONENT_INTERACTION_OPTIONS
    = COMMON_ATTACK
    | OPPONENT_COMMON_ATTACK
    | ENLIGHTENMENT_SPELL
    | OPPONENT_ENLIGHTENMENT_SPELL


playerCreationFunc : String -> String -> Player
playerCreationFunc elem pname =
    { location = Grid.Coordinate 10 10
    , textAvatar = elem
    , name = pname
    , direction = Down
    , health = 30
    , indexOfLight = 10
    , energy = 10
    , mana = 100
    , enlSpellEffect = IncreaseIndexOfLight
    , inventory = Dict.empty
    , hunger = 10
    , stealth = 20
    , armor = 1
    , protection = 5
    , coordination = 100
    , power = 2
    , initiative = 2 -- this will be altered by generating a random int between 1 and 100
    , indexOfLightMax = 20
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
    , enlSpellEffect = DecreaseHealth
    , inventory = Dict.empty
    , stealth = 20
    , armor = 1
    , protection = 5
    , coordination = 100
    , power = 2
    , initiative = 1 -- this will be altered by generating a random int between 1 and 100
    , attacksUsingGameOfThorns = True
    , indexOfLightMax = 11
    , disappearsWhenHealthIsZero = False
    , playerCanWalkOverIfDead = True
    , disappearsWhenIndexOfLightMax = False
    , maxNrEnemyMovesPerTurn = 2 -- to prevent possible infinite recursion in ai
    , nrMovesInCurrentTurn = 0
    , placed = False
    }
