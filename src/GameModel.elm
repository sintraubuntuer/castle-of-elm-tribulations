module GameModel exposing (ColumnInfo, DoorInfo, Enemy, EnemyId, FlagCondition(..), FlagInfo, FloorInfo, FloorStore, Input(..), Item(..), LeverId, LeverInfo, Location, Player, RoomRectangle, RoomsInfo, Size, StairsInfo, State, Tile(..), TunnelRectangle, Visibility(..), WallInfo, WallJunction(..), WallOverInfo, WaterInfo, defaultColumnInfo, defaultDoorInfo, defaultFlagInfo, defaultFloorInfo, defaultLeverInfo, defaultOrangeFloorInfo, defaultWallInfo, defaultWallUpInfo, defaultWaterInfo, enemy, getCurrentFloorInfoToStore, getGridTileVisibility, getModelTileVisibility, getRoomBottomY, getRoomCenterX, getRoomCenterY, getRoomLeftX, getRoomRightX, getRoomTopY, getTileVisibility, isFloor, isHorizontalWall, isMbTileHorizontalToTheLeft, isMbTileHorizontalToTheRight, isMbTileHorizontalWall, isMbTileVerticalWall, isMbTileWall, isModelTileExplored, isModelTileTransparent, isModelTileWalkable, isNoTileYet, isTileExplored, isTileTransparent, isTileWalkable, isVerticalWall, isWall, location, mbUpdateEnemyInitiativeByMbEnemyId, mbUpdateEnemyLocation, placeExistingEnemy, player, randomlyPlaceExistingEnemies, setModelTileAsExplored, setModelTileVisibility, setTileAsExplored, setTileVisibility, setWallTileOrientation, showTile, tupleFloatsToLocation, tupleIntsToLocation, validLocation, visibility, visible)

--import Generator
--import Generator.Standard
--import Keyboard
--import Element

import Collage.Text as Text
import Dict exposing (Dict)
import Grid


type alias EnemyId =
    Int


type alias LeverId =
    Int


type Tile
    = Floor FloorInfo
    | Stairs StairsInfo
    | Wall WallInfo
    | WallOver WallOverInfo
    | Door DoorInfo
    | Lever LeverInfo
    | Flag FlagInfo
    | Column ColumnInfo
    | Water WaterInfo
    | NoTileYet


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
    , wallPercentage : Maybe Float
    , roomsInfo : Maybe RoomsInfo
    , floorDict : Dict Int FloorStore
    , currentFloorId : Int
    , started : Bool
    }


type alias FloorStore =
    { level : Grid.Grid Tile
    , explored : Grid.Grid Visibility
    , window_width : Int
    , window_height : Int
    , total_width : Int
    , total_height : Int
    }


getCurrentFloorInfoToStore : State -> FloorStore
getCurrentFloorInfoToStore state =
    { level = state.level
    , explored = state.explored
    , window_width = state.window_width
    , window_height = state.window_height
    , total_width = state.total_width
    , total_height = state.total_height
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


getRoomLeftX : RoomRectangle -> Int
getRoomLeftX roomrectangle =
    roomrectangle.top_left_x


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
    , textAvatar : String --Element.Element
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
    , textAvatar : String --Element.Element
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
    | Ash


type alias FloorInfo =
    { item : Maybe Item
    , isTransparent : Bool
    , isWalkable : Bool
    , isExplored : Bool
    , visibility : Visibility
    , color : String
    }


type alias StairsInfo =
    { stairsId : Int
    , toFloorId : Int
    , toStairsId : Int
    , shift : ( Int, Int ) --after the player has been moved to the destination stairs what's the shift applied relative to the stairs position
    , isExplored : Bool
    , visibility : Visibility
    }


defaultFloorInfo : FloorInfo
defaultFloorInfo =
    { item = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored, color = "default" }


defaultOrangeFloorInfo : FloorInfo
defaultOrangeFloorInfo =
    { item = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored, color = "orange" }


type alias WallInfo =
    { isExplored : Bool
    , visibility : Visibility
    , orientation : String
    }


defaultWallInfo : WallInfo
defaultWallInfo =
    { isExplored = False, visibility = Unexplored, orientation = "horizontal" }


defaultWallUpInfo : WallInfo
defaultWallUpInfo =
    { isExplored = False, visibility = Unexplored, orientation = "up" }


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
    | FloorUp
    | FloorDown
    | Nop


player : String -> String -> Player
player elem pname =
    { location = Grid.Coordinate 10 10
    , textAvatar = elem
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


enemy : String -> EnemyId -> String -> Enemy
enemy elem enemyid ename =
    { location = Grid.Coordinate 14 4
    , id = enemyid
    , textAvatar = elem
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
validLocation location_ state =
    Grid.inGrid location_ state.level


isFloor : Tile -> Bool
isFloor tile =
    case tile of
        Floor _ ->
            True

        _ ->
            False


isWall : Tile -> Bool
isWall tile =
    case tile of
        Wall _ ->
            True

        _ ->
            False


isStairs : Tile -> Bool
isStairs tile =
    case tile of
        Stairs _ ->
            True

        _ ->
            False


isMbTileWall : Maybe Tile -> Bool
isMbTileWall mbtile =
    case mbtile of
        Just (Wall _) ->
            True

        _ ->
            False


isHorizontalWall : Tile -> Bool
isHorizontalWall tile =
    case tile of
        Wall infoRec ->
            infoRec.orientation == "horizontal"

        _ ->
            False


isMbTileHorizontalWall : Maybe Tile -> Bool
isMbTileHorizontalWall mbTile =
    case mbTile of
        Just (Wall infoRec) ->
            infoRec.orientation == "horizontal"

        _ ->
            False


isMbTileHorizontalToTheLeft : Maybe Tile -> Bool
isMbTileHorizontalToTheLeft mbTile =
    case mbTile of
        Just (Wall infoRec) ->
            infoRec.orientation == "horizontal" || infoRec.orientation == "bottom_right_corner" || infoRec.orientation == "top_right_corner"

        _ ->
            False


isMbTileHorizontalToTheRight : Maybe Tile -> Bool
isMbTileHorizontalToTheRight mbTile =
    case mbTile of
        Just (Wall infoRec) ->
            infoRec.orientation == "horizontal" || infoRec.orientation == "bottom_left_corner" || infoRec.orientation == "top_left_corner"

        _ ->
            False


isVerticalWall : Tile -> Bool
isVerticalWall tile =
    case tile of
        Wall infoRec ->
            infoRec.orientation == "up"

        _ ->
            False


isMbTileVerticalWall : Maybe Tile -> Bool
isMbTileVerticalWall mbTile =
    case mbTile of
        Just (Wall infoRec) ->
            infoRec.orientation == "up"

        _ ->
            False


setWallTileOrientation : String -> Tile -> Tile
setWallTileOrientation orientationStr tile =
    case tile of
        Wall winfo ->
            Wall { winfo | orientation = orientationStr }

        _ ->
            tile


isNoTileYet : Tile -> Bool
isNoTileYet tile =
    case tile of
        NoTileYet ->
            True

        _ ->
            False



--  = Floor FloorInfo
--  | Wall WallInfo
--  | WallOver WallOverInfo
--  | Door DoorInfo
--  | Lever LeverInfo
--  | Flag FlagInfo
--  | Column ColumnInfo
--  | Water WaterInfo
--  | NoTileYet


isTileWalkable : Tile -> Bool
isTileWalkable tile =
    case tile of
        Floor floorinfo ->
            if floorinfo.isWalkable then
                True

            else
                False

        Stairs sinfo ->
            True

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
isModelTileWalkable location_ state =
    Grid.get location_ state.level
        |> Maybe.map isTileWalkable
        |> Maybe.withDefault False


isTileTransparent : Tile -> Bool
isTileTransparent tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isTransparent

        Stairs sinfo ->
            False

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
isModelTileTransparent location_ state =
    Grid.get location_ state.level
        |> Maybe.map isTileTransparent
        |> Maybe.withDefault False


isTileExplored : Tile -> Bool
isTileExplored tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isExplored

        Stairs sinfo ->
            sinfo.isExplored

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
isModelTileExplored location_ state =
    Grid.get location_ state.level
        |> Maybe.map isTileExplored
        |> Maybe.withDefault False


setTileAsExplored : Tile -> Tile
setTileAsExplored tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | isExplored = True }

        Stairs sinfo ->
            Stairs { sinfo | isExplored = True }

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
setModelTileAsExplored location_ state =
    case Grid.get location_ state.level of
        Nothing ->
            state

        Just tile ->
            let
                newTile =
                    setTileAsExplored tile
            in
            { state | level = Grid.set location_ newTile state.level }


getTileVisibility : Tile -> Visibility
getTileVisibility tile =
    case tile of
        Floor floorinfo ->
            floorinfo.visibility

        Stairs sinfo ->
            sinfo.visibility

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
getGridTileVisibility location_ gridtiles =
    Grid.get location_ gridtiles
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Unexplored


getModelTileVisibility : Location -> State -> Visibility
getModelTileVisibility location_ state =
    Grid.get location_ state.level
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Unexplored


setTileVisibility : Visibility -> Tile -> Tile
setTileVisibility visibility_ tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | visibility = visibility_ }

        Stairs sinfo ->
            Stairs { sinfo | visibility = visibility_ }

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
setModelTileVisibility location_ visibility_ state =
    case Grid.get location_ state.level of
        Nothing ->
            state

        Just tile ->
            let
                newTile =
                    setTileVisibility visibility_ tile
            in
            { state | level = Grid.set location_ newTile state.level }



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
                    Dict.update enemyid (\mbEnemy -> mbEnemy |> Maybe.map (\enemyRec -> { enemyRec | initiative = intval })) state.enemies
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

        Just enemy_ ->
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


showTile : Tile -> Text.Text
showTile tile =
    let
        c =
            case tile of
                Floor floorinfo ->
                    " "

                Stairs sinfo ->
                    "/"

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
    c
        |> Text.fromString



--|> Text.monospace
--Element.centered << Text.monospace << Text.fromString <| c


visible : State -> List Location
visible state =
    Grid.neighborhoodCalc 8 state.player.location


visibility : State -> Location -> Visibility
visibility state location_ =
    --Grid.getWithDefault Unexplored location state.explored
    getModelTileVisibility location_ state
