module GameModel exposing
    ( ColumnInfo
    , DoorInfo
    , DoorOrientation(..)
    , DoorWallOption(..)
    , DoorWallOptions
    , FlagCondition(..)
    , FlagInfo
    , FloorDrawing(..)
    , FloorInfo
    , FloorStore
    , GrassInfo
    , HoleInfo
    , Input(..)
    , LeverId
    , LeverInfo
    , Location
    , Model
    , RoomRectangle
    , RoomType(..)
    , RoomsInfo
    , StairsInfo
    , TeleporterInfo
    , TeleporterType(..)
    , Tile(..)
    , TunnelRectangle
    , Visibility(..)
    , WallInfo
    , WallJunction(..)
    , WallOverInfo
    , WaterInfo
    , defaultBlackDoorInfo
    , defaultBlueDoorInfo
    , defaultBrickWallInfo
    , defaultColumnInfo
    , defaultDoorInfo
    , defaultFlagInfo
    , defaultFloorInfo
    , defaultGrassInfo
    , defaultGrassWithDirtInfo
    , defaultGreenDoorInfo
    , defaultLeverInfo
    , defaultOpenDoorInfo
    , defaultOrangeFloorInfo
    , defaultRedDoorInfo
    , defaultWallInfo
    , defaultWallUpInfo
    , defaultWaterInfo
    , defaultWaterWallLeftInfo
    , defaultWaterWallUpInfo
    , defaultYellowDoorInfo
    , getCurrentFloorInfoToStore
    , getGridTileVisibility
    , getModelTileVisibility
    , getRoomBottomY
    , getRoomCenterX
    , getRoomCenterY
    , getRoomLeftX
    , getRoomRightX
    , getRoomTopY
    , getTileVisibility
    , isFloor
    , isHorizontalWall
    , isMbTileHorizontalToTheLeft
    , isMbTileHorizontalToTheRight
    , isMbTileHorizontalWall
    , isMbTileVerticalWall
    , isMbTileWall
    , isModelTileExplored
    , isModelTileTransparent
    , isModelTileWalkable
    , isNoTileYet
    , isTileExplored
    , isTileTransparent
    , isTileWalkable
    , isVerticalWall
    , isWall
    , itemToString
    , location
    , mbUpdateEnemyInitiativeByMbEnemyId
    , mbUpdateEnemyLocation
    , placeExistingEnemy
    , randomlyPlaceExistingEnemies
    , setModelTileAsExplored
    , setModelTileVisibility
    , setTileAsExplored
    , setTileVisibility
    , setWallTileOrientation
    , showTile
    , tupleFloatsToLocation
    , tupleIntsToLocation
    , validLocation
    , visibility
    , visible
    )

import Beings
    exposing
        ( CharacterId
        , Direction(..)
        , Enemy
        , EnemyId
        , OPPONENT_INTERACTION_OPTIONS(..)
        , OtherCharacter
        , Player
        )
import Collage.Text as Text
import Dict exposing (Dict)
import Grid
import Item exposing (Item(..), KeyInfo)
import Thorns.Types


type alias LeverId =
    Int


type Tile
    = Floor FloorInfo
    | Stairs StairsInfo
    | Hole HoleInfo
    | Wall WallInfo
    | WallOver WallOverInfo
    | Door DoorInfo
    | Lever LeverInfo
    | Flag FlagInfo
    | Column ColumnInfo
    | Water WaterInfo
    | Grass GrassInfo
    | NoTileYet


type alias GrassInfo =
    { description : String
    , isTransparent : Bool
    , isWalkable : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


type alias Model =
    { player : Player
    , enemies : Dict EnemyId Enemy
    , otherCharacters : Dict CharacterId OtherCharacter
    , level : Grid.Grid Tile
    , explored : Grid.Grid Visibility
    , log : List String
    , gameOfThornsModel : Thorns.Types.Model
    , gameOfThornsModeisOn : Bool
    , listeningToKeyInput : Bool
    , pseudoRandomIntsPool : List Int
    , x_display_anchor : Int
    , y_display_anchor : Int
    , window_width : Int
    , window_height : Int
    , total_width : Int
    , total_height : Int
    , displayInventory : Bool
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


getCurrentFloorInfoToStore : Model -> FloorStore
getCurrentFloorInfoToStore model =
    { level = model.level
    , explored = model.explored
    , window_width = model.window_width
    , window_height = model.window_height
    , total_width = model.total_width
    , total_height = model.total_height
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


type alias HoleInfo =
    { holeId : Int
    , floorId : Int
    , x : Int
    , y : Int
    , target_id : Int
    , isExplored : Bool
    , visibility : Visibility
    }


type RoomType
    = SquareRoom
    | HorizontalRoom
    | VerticalRoom


type TeleporterType
    = Barrel
    | BookCase
    | Clock


type alias TeleporterInfo =
    { teleporter_id : Int
    , floor_id : Int
    , teleporterType : TeleporterType
    , room_row_nr : Int
    , room_col_nr : Int
    , room_type : RoomType
    , position_in_room : String --(in WallUp , L R or D )
    , target_id : Int
    , shift : ( Int, Int )
    , isExplored : Bool
    , visibility : Visibility
    }



-- RoomsInfo [] 20 13 8
-- 20 13 8
{- }

   type alias Player =
       { location : Location
       , textAvatar : String --Element.Element
       , name : String
       , health : Int
       , energy : Int
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
-}


type alias Inventory =
    Dict String Item



{- }

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
-}


type alias Location =
    Grid.Coordinate


tupleIntsToLocation : ( Int, Int ) -> Location
tupleIntsToLocation ( x, y ) =
    Grid.Coordinate x y


tupleFloatsToLocation : ( Float, Float ) -> Location
tupleFloatsToLocation ( x, y ) =
    Grid.Coordinate (round x) (round y)


itemToString : Item -> String
itemToString item =
    case item of
        Chest s ->
            "chest_" ++ String.fromInt s

        Skull ->
            "skull"

        Key kinfo ->
            "key_" ++ kinfo.keyColor

        Money ->
            "money"

        Box ->
            "box"

        Ash ->
            "ash"


type FloorDrawing
    = LandingTargetDrawing Int


type alias FloorInfo =
    { item : Maybe Item
    , floorDrawing : Maybe FloorDrawing
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
    { item = Nothing, floorDrawing = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored, color = "default" }


defaultOrangeFloorInfo : FloorInfo
defaultOrangeFloorInfo =
    { item = Nothing, floorDrawing = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored, color = "orange" }


type alias WallInfo =
    { isExplored : Bool
    , visibility : Visibility
    , orientation : String
    , mbTeleporterObject : Maybe TeleporterInfo
    }


defaultBrickWallInfo : WallInfo
defaultBrickWallInfo =
    { isExplored = False, visibility = Unexplored, orientation = "just_bricks", mbTeleporterObject = Nothing }


defaultWallInfo : WallInfo
defaultWallInfo =
    { isExplored = False, visibility = Unexplored, orientation = "horizontal", mbTeleporterObject = Nothing }


defaultWallUpInfo : WallInfo
defaultWallUpInfo =
    { isExplored = False, visibility = Unexplored, orientation = "up", mbTeleporterObject = Nothing }


defaultGrassWithDirtInfo : GrassInfo
defaultGrassWithDirtInfo =
    { description = "grass_with_dirt", isTransparent = False, isWalkable = True, isExplored = False, visibility = Unexplored }


defaultGrassInfo : GrassInfo
defaultGrassInfo =
    { description = "grass", isTransparent = False, isWalkable = True, isExplored = False, visibility = Unexplored }


type DoorWallOption
    = UseDoor DoorInfo
    | UseWall
    | NoDoorNoWall


type alias DoorWallOptions =
    { left : DoorWallOption
    , top : DoorWallOption
    , right : DoorWallOption
    , bottom : DoorWallOption
    }


type DoorOrientation
    = DoorToTheRight
    | DoorToTheLeft
    | DoorToUp
    | DoorToDown


type alias DoorInfo =
    { isOpen : Bool
    , color : Maybe String
    , orientation : DoorOrientation
    , requiresToOpen : Maybe Item
    , isExplored : Bool
    , visibility : Visibility
    }


defaultDoorInfo : DoorOrientation -> DoorInfo
defaultDoorInfo dorientation =
    { isOpen = False, color = Nothing, orientation = dorientation, requiresToOpen = Nothing, isExplored = False, visibility = Unexplored }


defaultOpenDoorInfo : DoorOrientation -> DoorInfo
defaultOpenDoorInfo dorientation =
    { isOpen = True, color = Nothing, orientation = dorientation, requiresToOpen = Nothing, isExplored = False, visibility = Unexplored }


defaultBlueDoorInfo : DoorOrientation -> DoorInfo
defaultBlueDoorInfo dorientation =
    { isOpen = False, color = Just "blue", orientation = dorientation, requiresToOpen = Just (Key { keyColor = "blue" }), isExplored = False, visibility = Unexplored }


defaultBlackDoorInfo : DoorOrientation -> DoorInfo
defaultBlackDoorInfo dorientation =
    { isOpen = False, color = Just "black", orientation = dorientation, requiresToOpen = Just (Key { keyColor = "black" }), isExplored = False, visibility = Unexplored }


defaultRedDoorInfo : DoorOrientation -> DoorInfo
defaultRedDoorInfo dorientation =
    { isOpen = False, color = Just "red", orientation = dorientation, requiresToOpen = Just (Key { keyColor = "red" }), isExplored = False, visibility = Unexplored }


defaultYellowDoorInfo : DoorOrientation -> DoorInfo
defaultYellowDoorInfo dorientation =
    { isOpen = False, color = Just "yellow", orientation = dorientation, requiresToOpen = Just (Key { keyColor = "yellow" }), isExplored = False, visibility = Unexplored }


defaultGreenDoorInfo : DoorOrientation -> DoorInfo
defaultGreenDoorInfo dorientation =
    { isOpen = False, color = Just "green", orientation = dorientation, requiresToOpen = Just (Key { keyColor = "green" }), isExplored = False, visibility = Unexplored }


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
    { description : String
    , isTransparent : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


defaultWaterWallUpInfo : WaterInfo
defaultWaterWallUpInfo =
    { description = "water_wall_up", isTransparent = False, isExplored = False, visibility = Unexplored }


defaultWaterWallLeftInfo : WaterInfo
defaultWaterWallLeftInfo =
    { description = "water_wall_left", isTransparent = False, isExplored = False, visibility = Unexplored }


defaultWaterInfo : WaterInfo
defaultWaterInfo =
    { description = "just_water", isTransparent = False, isExplored = False, visibility = Unexplored }


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
    | PickUpItem
    | ViewInventory
    | Nop


location : Int -> Int -> Location
location =
    Grid.Coordinate


validLocation : Location -> Model -> Bool
validLocation location_ model =
    Grid.inGrid location_ model.level


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


isGrass : Tile -> Bool
isGrass tile =
    case tile of
        Grass _ ->
            True

        _ ->
            False


isHole : Tile -> Bool
isHole tile =
    case tile of
        Hole _ ->
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


isTileWalkable : Player -> Tile -> Bool
isTileWalkable player_ tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isWalkable

        Grass grassinfo ->
            grassinfo.isWalkable

        Stairs sinfo ->
            True

        Hole hinfo ->
            True

        Wall wInfo ->
            case wInfo.mbTeleporterObject of
                Just tel ->
                    True

                Nothing ->
                    False

        WallOver wOverInfo ->
            False

        Door doorinfo ->
            --doorinfo.isOpen || List.contains doorinfo.requiresToOpen (Dict.values player.inventory)
            case doorinfo.requiresToOpen of
                Nothing ->
                    True

                Just item ->
                    List.filter (\it -> it == item) (Dict.values player_.inventory)
                        |> List.head
                        |> (\x -> x /= Nothing)

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


isModelTileWalkable : Location -> Model -> Bool
isModelTileWalkable location_ model =
    Grid.get location_ model.level
        |> Maybe.map (isTileWalkable model.player)
        |> Maybe.withDefault False


isTileTransparent : Tile -> Bool
isTileTransparent tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isTransparent

        Grass grassinfo ->
            grassinfo.isTransparent

        Stairs sinfo ->
            False

        Hole hinfo ->
            True

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


isModelTileTransparent : Location -> Model -> Bool
isModelTileTransparent location_ model =
    Grid.get location_ model.level
        |> Maybe.map isTileTransparent
        |> Maybe.withDefault False


isTileExplored : Tile -> Bool
isTileExplored tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isExplored

        Grass ginfo ->
            ginfo.isExplored

        Stairs sinfo ->
            sinfo.isExplored

        Hole hinfo ->
            hinfo.isExplored

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


isModelTileExplored : Location -> Model -> Bool
isModelTileExplored location_ model =
    Grid.get location_ model.level
        |> Maybe.map isTileExplored
        |> Maybe.withDefault False


setTileAsExplored : Tile -> Tile
setTileAsExplored tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | isExplored = True }

        Grass grassinfo ->
            Grass { grassinfo | isExplored = True }

        Stairs sinfo ->
            Stairs { sinfo | isExplored = True }

        Hole hinfo ->
            Hole { hinfo | isExplored = True }

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


setModelTileAsExplored : Location -> Model -> Model
setModelTileAsExplored location_ model =
    case Grid.get location_ model.level of
        Nothing ->
            model

        Just tile ->
            let
                newTile =
                    setTileAsExplored tile
            in
            { model | level = Grid.set location_ newTile model.level }


getTileVisibility : Tile -> Visibility
getTileVisibility tile =
    case tile of
        Floor floorinfo ->
            floorinfo.visibility

        Grass grassinfo ->
            grassinfo.visibility

        Stairs sinfo ->
            sinfo.visibility

        Hole hinfo ->
            hinfo.visibility

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


getModelTileVisibility : Location -> Model -> Visibility
getModelTileVisibility location_ model =
    Grid.get location_ model.level
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Unexplored


setTileVisibility : Visibility -> Tile -> Tile
setTileVisibility visibility_ tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | visibility = visibility_ }

        Grass grassinfo ->
            Grass { grassinfo | visibility = visibility_ }

        Stairs sinfo ->
            Stairs { sinfo | visibility = visibility_ }

        Hole hinfo ->
            Hole { hinfo | visibility = visibility_ }

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


setModelTileVisibility : Location -> Visibility -> Model -> Model
setModelTileVisibility location_ visibility_ model =
    case Grid.get location_ model.level of
        Nothing ->
            model

        Just tile ->
            let
                newTile =
                    setTileVisibility visibility_ tile
            in
            { model | level = Grid.set location_ newTile model.level }



{-
   getRandomPathable : Model  -> ( Location, Model  )
   getRandomPathable model =
       let
           ( x, gen' ) =
               Generator.int32Range ( 1, model.level.size.width ) model.generator

           ( y, gen'' ) =
               Generator.int32Range ( 1, model.level.size.height ) gen'

           locn =
               location x y

           model' =
               { model | generator = gen'' }
       in
       case pathable locn model' of
           True ->
               ( locn, model' )

           False ->
               getRandomPathable model'



-}


mbUpdateEnemyInitiativeByMbEnemyId : Int -> Maybe EnemyId -> Model -> Model
mbUpdateEnemyInitiativeByMbEnemyId intval mbEnemyid model =
    case mbEnemyid of
        Nothing ->
            model

        Just enemyid ->
            let
                newEnemies =
                    Dict.update enemyid (\mbEnemy -> mbEnemy |> Maybe.map (\enemyRec -> { enemyRec | initiative = intval })) model.enemies
            in
            { model | enemies = newEnemies }


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


randomlyPlaceExistingEnemies : List ( Location, EnemyId ) -> Model -> Model
randomlyPlaceExistingEnemies lpairIntIds model =
    let
        dictenemies =
            model.enemies

        newDictEnemies =
            List.foldl (\( loc, enid ) dictacc -> placeExistingEnemy enid loc dictacc) dictenemies lpairIntIds
    in
    { model | enemies = newDictEnemies }


showTile : Tile -> Text.Text
showTile tile =
    let
        c =
            case tile of
                Floor floorinfo ->
                    " "

                Grass ginfo ->
                    "g"

                Stairs sinfo ->
                    "/"

                Hole hinfo ->
                    "h"

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


visible : Model -> List Location
visible model =
    Grid.neighborhoodCalc 8 model.player.location


visibility : Model -> Location -> Visibility
visibility model location_ =
    --Grid.getWithDefault Unexplored location model.explored
    getModelTileVisibility location_ model
