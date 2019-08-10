module Beings exposing
    ( Enemy
    , EnemyId
    , Inventory
    , Location
    , OPPONENT_INTERACTION_OPTIONS(..)
    , Player
    )

import Dict exposing (Dict)
import Grid
import Item exposing (Item, KeyInfo)


type alias Location =
    Grid.Coordinate


type alias EnemyId =
    Int



{- }
   let COMBAT_OPTIONS = {
   	[ATTACK_1]: 2,
   	[ATTACK_2]: 2,
   	[MAGIC_1]: 2,
   	[MAGIC_2]: 2
   };

-}


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


type alias Inventory =
    Dict String Item


type OPPONENT_INTERACTION_OPTIONS
    = CHICANE_ATTACK
    | OPPONENT_CHICANE_ATTACK
    | ENLIGHTENMENT_SPELL
    | OPPONENT_ENLIGHTENMENT_SPELL
