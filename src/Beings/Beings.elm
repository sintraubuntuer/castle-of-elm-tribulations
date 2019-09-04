module Beings.Beings exposing
    ( CharacterId
    , Direction(..)
    , EnlightenmentSpellEffect(..)
    , FightingCharacter
    , FightingCharacterId
    , Inventory
    , Location
    , MovingStrategy(..)
    , OPPONENT_INTERACTION_OPTIONS(..)
    , OtherCharacter
    , OtherCharacterId
    , Player
    , fightingCharacterCreationFunc
    , otherCharacterCreationFunc
    , playerCreationFunc
    )

import Dict exposing (Dict)
import Grid3 as Grid
import Item exposing (Item, KeyInfo)


type alias Location =
    Grid.Coordinate


type alias FightingCharacterId =
    Int


type alias CharacterId =
    Int


type alias OtherCharacterId =
    Int


type EnlightenmentSpellEffect
    = DecreaseHealth
    | IncreaseIndexOfLight
    | DecreaseIndexOfLight


type alias Player =
    { location : Location
    , textAvatar : String
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
    , movingStrategy : Maybe MovingStrategy
    , placed : Bool
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type MovingStrategy
    = MoveTowardsPlayer
    | CustomMoveTowardsPlayerBeforeAndAfterEnl Float Float
    | MoveRandomly
    | DontMove


type alias FightingCharacter =
    { location : Location
    , id : FightingCharacterId
    , textAvatar : String
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
    , movingStrategy : Maybe MovingStrategy
    , disappearsWhenHealthIsZero : Bool
    , playerCanWalkOverIfDead : Bool
    , disappearsWhenIndexOfLightMax : Bool
    , maxNrCharacterMovesPerTurn : Int
    , nrMovesInCurrentTurn : Int
    , placed : Bool
    }


type alias OtherCharacter =
    { location : Location
    , id : OtherCharacterId
    , textAvatar : String
    , name : String
    , direction : Direction
    , species : String
    , health : Int
    , indexOfLight : Int
    , mana : Int
    , enlSpellEffect : EnlightenmentSpellEffect
    , inventory : Inventory
    , initiative : Int
    , indexOfLightMax : Int
    , movingStrategy : Maybe MovingStrategy
    , disappearsWhenHealthIsZero : Bool
    , playerCanWalkOverIfDead : Bool
    , disappearsWhenIndexOfLightMax : Bool
    , maxNrCharacterMovesPerTurn : Int
    , nrMovesInCurrentTurn : Int
    , placed : Bool
    }


otherCharacterCreationFunc : Int -> String -> Int -> Int -> Int -> OtherCharacter
otherCharacterCreationFunc id_ ename x_coord y_coord floor_id_ =
    { location = Grid.Coordinate x_coord y_coord floor_id_
    , id = id_
    , textAvatar = ""
    , name = ename
    , direction = Down
    , species = "otherYou"
    , health = 30
    , indexOfLight = 8
    , mana = 100
    , enlSpellEffect = IncreaseIndexOfLight
    , inventory = Dict.empty
    , initiative = 100
    , indexOfLightMax = 30
    , movingStrategy = Just DontMove
    , disappearsWhenHealthIsZero = True
    , playerCanWalkOverIfDead = True
    , disappearsWhenIndexOfLightMax = False
    , maxNrCharacterMovesPerTurn = 1
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


playerCreationFunc : String -> String -> Int -> Int -> Int -> Player
playerCreationFunc elem pname x_coord y_coord floor_nr =
    { location = Grid.Coordinate x_coord y_coord floor_nr
    , textAvatar = elem
    , name = pname
    , direction = Down
    , health = 30
    , indexOfLight = 10
    , energy = 10
    , mana = 200
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
    , movingStrategy = Nothing
    , placed = False
    }


fightingCharacterCreationFunc : String -> FightingCharacterId -> String -> String -> Int -> Int -> Int -> FightingCharacter
fightingCharacterCreationFunc elem fcharId ename species_ x_coord y_coord floor_id =
    { location = Grid.Coordinate x_coord y_coord floor_id
    , id = fcharId
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
    , movingStrategy = Just (CustomMoveTowardsPlayerBeforeAndAfterEnl 0.85 0.45)
    , disappearsWhenHealthIsZero = False
    , playerCanWalkOverIfDead = True
    , disappearsWhenIndexOfLightMax = False
    , maxNrCharacterMovesPerTurn = 2
    , nrMovesInCurrentTurn = 0
    , placed = False
    }
