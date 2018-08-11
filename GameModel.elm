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
    , x_display_anchor : Int
    , y_display_anchor : Int
    , window_width : Int
    , window_height : Int
    , total_width : Int
    , total_height : Int
    , roomsInfo : RoomsInfo
    }


type alias RoomRectangle =
    { top_left_x : Int
    , top_left_y : Int
    , width : Int
    , height : Int
    }


type alias TunnelRectangle =
    { top_left_x : Int
    , top_left_y : Int
    , width : Int
    , height : Int
    }


getRoomCenterX : RoomRectangle -> Int
getRoomCenterX roomrectangle =
    roomrectangle.top_left_x + round (toFloat roomrectangle.width / 2.0)


getRoomRightX : RoomRectangle -> Int
getRoomRightX roomrectangle =
    roomrectangle.top_left_x + roomrectangle.width - 1


getRoomCenterY : RoomRectangle -> Int
getRoomCenterY roomrectangle =
    roomrectangle.top_left_y + round (toFloat roomrectangle.height / 2.0)


getRoomBottomY : RoomRectangle -> Int
getRoomBottomY roomrectangle =
    roomrectangle.top_left_y + roomrectangle.height - 1


getRoomTopY : RoomRectangle -> Int
getRoomTopY roomrectangle =
    roomrectangle.top_left_y


type alias RoomsInfo =
    { roomRectangles : List RoomRectangle
    , maxNrOfRooms : Int
    , maxRoomSize : Int
    , minRoomSize : Int
    }



-- RoomsInfo [] 20 13 8
-- 20 13 8


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


tupleIntsToLocation : ( Int, Int ) -> Location
tupleIntsToLocation ( x, y ) =
    Grid.Coordinate x y


tupleFloatsToLocation : ( Float, Float ) -> Location
tupleFloatsToLocation ( x, y ) =
    Grid.Coordinate (round x) (round y)


type alias Size =
    Int


type Item
    = Chest Size
    | Skull
    | Key
    | Money
    | Box


type alias FloorInfo =
    { items : List Item
    , isTransparent : Bool
    , isWalkable : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


defaultFloorInfo : FloorInfo
defaultFloorInfo =
    { items = [], isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored }


type alias WallInfo =
    { isExplored : Bool
    , visibility : Visibility
    }


defaultWallInfo : WallInfo
defaultWallInfo =
    { isExplored = False, visibility = Unexplored }


type alias DoorInfo =
    { isOpen : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


defaultDoorInfo : DoorInfo
defaultDoorInfo =
    { isOpen = False, isExplored = False, visibility = Unexplored }


type alias LeverInfo =
    { isUp : Bool
    , isTransparent : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


defaultLeverInfo : LeverInfo
defaultLeverInfo =
    { isUp = False, isTransparent = False, isExplored = False, visibility = Unexplored }


type alias FlagInfo =
    { condition : FlagCondition
    , isTransparent : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


defaultFlagInfo : FlagInfo
defaultFlagInfo =
    { condition = Fine, isTransparent = False, isExplored = False, visibility = Unexplored }


type FlagCondition
    = Ruined
    | Fine


type alias ColumnInfo =
    { isExplored : Bool
    , visibility : Visibility
    }


defaultColumnInfo : ColumnInfo
defaultColumnInfo =
    { isExplored = False, visibility = Unexplored }


type alias WaterInfo =
    { isTransparent : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


defaultWaterInfo : WaterInfo
defaultWaterInfo =
    { isTransparent = False, isExplored = False, visibility = Unexplored }


type Tile
    = Floor FloorInfo
    | Wall WallInfo
    | WallOver WallOverInfo
    | Door DoorInfo
    | Lever LeverInfo
    | Flag FlagInfo
    | Column ColumnInfo
    | Water WaterInfo
    | NoTileYet


type WallJunction
    = Flat
    | Empty


type alias WallOverInfo =
    { r : WallJunction, l : WallJunction, u : WallJunction, d : WallJunction, isExplored : Bool, visibility : Visibility }


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


isTileWalkable : Tile -> Bool
isTileWalkable tile =
    case tile of
        Floor floorinfo ->
            if floorinfo.isWalkable then
                True
            else
                False

        Wall wInfo ->
            False

        WallOver wOverInfo ->
            False

        Door doorinfo ->
            if doorinfo.isOpen then
                True
            else
                False

        Lever leverInfo ->
            False

        Flag flagInfo ->
            False

        Column columnInfo ->
            False

        Water waterInfo ->
            False

        NoTileYet ->
            False


isModelTileWalkable : Location -> State -> Bool
isModelTileWalkable location state =
    Grid.get location state.level
        |> Maybe.map isTileWalkable
        |> Maybe.withDefault False


isTileTransparent : Tile -> Bool
isTileTransparent tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isTransparent

        Wall wInfo ->
            False

        WallOver wOverInfo ->
            False

        Door doorinfo ->
            doorinfo.isOpen

        Lever leverInfo ->
            leverInfo.isTransparent

        Flag flagInfo ->
            flagInfo.isTransparent

        Column columnInfo ->
            False

        Water waterInfo ->
            waterInfo.isTransparent

        NoTileYet ->
            False


isModelTileTransparent : Location -> State -> Bool
isModelTileTransparent location state =
    Grid.get location state.level
        |> Maybe.map isTileTransparent
        |> Maybe.withDefault False


isTileExplored : Tile -> Bool
isTileExplored tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isExplored

        Wall wInfo ->
            wInfo.isExplored

        WallOver wOverInfo ->
            wOverInfo.isExplored

        Door doorinfo ->
            doorinfo.isExplored

        Lever leverInfo ->
            leverInfo.isExplored

        Flag flagInfo ->
            flagInfo.isExplored

        Column columnInfo ->
            columnInfo.isExplored

        Water waterInfo ->
            waterInfo.isExplored

        NoTileYet ->
            False


isModelTileExplored : Location -> State -> Bool
isModelTileExplored location state =
    Grid.get location state.level
        |> Maybe.map isTileExplored
        |> Maybe.withDefault False


setTileAsExplored : Tile -> Tile
setTileAsExplored tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | isExplored = True }

        Wall wallinfo ->
            Wall { wallinfo | isExplored = True }

        WallOver wOverInfo ->
            WallOver { wOverInfo | isExplored = True }

        Door doorinfo ->
            Door { doorinfo | isExplored = True }

        Lever leverinfo ->
            Lever { leverinfo | isExplored = True }

        Flag flagInfo ->
            Flag { flagInfo | isExplored = True }

        Column columnInfo ->
            Column { columnInfo | isExplored = True }

        Water waterinfo ->
            Water { waterinfo | isExplored = True }

        NoTileYet ->
            NoTileYet


setModelTileAsExplored : Location -> State -> State
setModelTileAsExplored location state =
    case Grid.get location state.level of
        Nothing ->
            state

        Just tile ->
            let
                newTile =
                    setTileAsExplored tile
            in
            { state | level = Grid.set location newTile state.level }


getTileVisibility : Tile -> Visibility
getTileVisibility tile =
    case tile of
        Floor floorinfo ->
            floorinfo.visibility

        Wall wInfo ->
            wInfo.visibility

        WallOver wOverInfo ->
            wOverInfo.visibility

        Door doorinfo ->
            doorinfo.visibility

        Lever leverInfo ->
            leverInfo.visibility

        Flag flagInfo ->
            flagInfo.visibility

        Column columnInfo ->
            columnInfo.visibility

        Water waterInfo ->
            waterInfo.visibility

        NoTileYet ->
            Unexplored


getGridTileVisibility : Location -> Grid.Grid Tile -> Visibility
getGridTileVisibility location gridtiles =
    Grid.get location gridtiles
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Unexplored


getModelTileVisibility : Location -> State -> Visibility
getModelTileVisibility location state =
    Grid.get location state.level
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Unexplored


setTileVisibility : Visibility -> Tile -> Tile
setTileVisibility visibility_ tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | visibility = visibility_ }

        Wall wInfo ->
            Wall { wInfo | visibility = visibility_ }

        WallOver wOverInfo ->
            WallOver { wOverInfo | visibility = visibility_ }

        Door doorinfo ->
            Door { doorinfo | visibility = visibility_ }

        Lever leverInfo ->
            Lever { leverInfo | visibility = visibility_ }

        Flag flagInfo ->
            Flag { flagInfo | visibility = visibility_ }

        Column columnInfo ->
            Column { columnInfo | visibility = visibility_ }

        Water waterInfo ->
            Water { waterInfo | visibility = visibility_ }

        NoTileYet ->
            NoTileYet


setModelTileVisibility : Location -> Visibility -> State -> State
setModelTileVisibility location visibility_ state =
    case Grid.get location state.level of
        Nothing ->
            state

        Just tile ->
            let
                newTile =
                    setTileVisibility visibility_ tile
            in
            { state | level = Grid.set location newTile state.level }



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
                Floor floorinfo ->
                    " "

                Wall winfo ->
                    "#"

                WallOver wj ->
                    "#"

                Door doorinfo ->
                    "+"

                NoTileYet ->
                    "n"

                _ ->
                    "na"
    in
    Element.centered << Text.monospace << Text.fromString <| c


visible : State -> List Location
visible state =
    Grid.neighborhoodCalc 4 state.player.location


visibility : State -> Location -> Visibility
visibility state location =
    --Grid.getWithDefault Unexplored location state.explored
    getModelTileVisibility location state
