module GameModel exposing
    ( CurrentDisplay(..)
    , DoorWallOption(..)
    , DoorWallOptions
    , FloorStore
    , Input(..)
    , Location
    , Model
    , ModelChangerFuncs(..)
    , RoomRectangle
    , RoomsInfo
    , TunnelRectangle
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
    , defaultPineTreeInfo
    , defaultRedDoorInfo
    , defaultRoundTreeInfo
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
    , isConverterTile
    ,  isFloor
       -- , isHorizontalWall
       --, isMbTileHorizontalToTheLeft
       --, isMbTileHorizontalToTheRight
       --, isMbTileHorizontalWall
       --, isMbTileVerticalWall

    , isMbTileWall
    , isModelTileExplored
    , isModelTileTransparent
    , isNoTileYet
    , isTileExplored
    ,  isTileTransparent
       -- , isVerticalWall

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
    , walkableWaterInfo
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
import Tile exposing (Tile(..))


type CurrentDisplay
    = DisplayRegularGame
    | DisplayGameOver
    | DisplayGameCompleted
    | DisplayGameOfThorns
    | DisplayOpponentReport
    | DisplayHelpScreen
    | DisplayInventory


type ModelChangerFuncs
    = SimpleModelChanger (List (Grid.Coordinate -> Model -> Model))


type alias Model =
    { player : Player
    , enemies : Dict EnemyId Enemy
    , otherCharacters : Dict CharacterId OtherCharacter
    , level : Grid.Grid Tile
    , explored : Grid.Grid Tile.Visibility
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
    , currentDisplay : CurrentDisplay
    , displayStatsOverlay : Bool
    , showBlood : Bool
    , wallPercentage : Maybe Float
    , roomsInfo : Maybe RoomsInfo
    , floorDict : Dict Int FloorStore
    , currentFloorId : Int
    , gameCompletionFunc : Int -> Grid.Coordinate -> Bool
    , leverModelChangerFuncs : Dict Tile.LeverId ModelChangerFuncs
    , started : Bool
    , debugMode : Bool
    }


type alias FloorStore =
    { level : Grid.Grid Tile
    , explored : Grid.Grid Tile.Visibility
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


type alias Inventory =
    Dict String Item


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

        Paper paperinfo ->
            "a piece of paper : " ++ paperinfo.description ++ " , with some written text : " ++ paperinfo.text

        Food fdescription ->
            "a piece of food : " ++ fdescription


defaultFloorInfo : Tile.FloorInfo
defaultFloorInfo =
    { item = Nothing, floorDrawing = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Tile.Unexplored, color = "default" }


defaultOrangeFloorInfo : Tile.FloorInfo
defaultOrangeFloorInfo =
    { item = Nothing, floorDrawing = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Tile.Unexplored, color = "orange" }


defaultBrickWallInfo : Tile.WallInfo
defaultBrickWallInfo =
    { isExplored = False, visibility = Tile.Unexplored, orientation = "just_bricks", mbTeleporterObject = Nothing }


defaultWallInfo : Tile.WallInfo
defaultWallInfo =
    { isExplored = False, visibility = Tile.Unexplored, orientation = "horizontal", mbTeleporterObject = Nothing }


defaultWallUpInfo : Tile.WallInfo
defaultWallUpInfo =
    { isExplored = False, visibility = Tile.Unexplored, orientation = "up", mbTeleporterObject = Nothing }


defaultGrassWithDirtInfo : Tile.GrassInfo
defaultGrassWithDirtInfo =
    { description = "grass_with_dirt", isTransparent = False, isWalkable = True, isExplored = False, visibility = Tile.Unexplored }


defaultGrassInfo : Tile.GrassInfo
defaultGrassInfo =
    { description = "grass", isTransparent = False, isWalkable = True, isExplored = False, visibility = Tile.Unexplored }


defaultPineTreeInfo : Tile.TreeInfo
defaultPineTreeInfo =
    { treeType = "pinetree", isExplored = False, visibility = Tile.Unexplored }


defaultRoundTreeInfo : Tile.TreeInfo
defaultRoundTreeInfo =
    { treeType = "roundtree", isExplored = False, visibility = Tile.Unexplored }


type DoorWallOption
    = UseDoor Tile.DoorInfo
    | UseWall
    | NoDoorNoWall


type alias DoorWallOptions =
    { left : DoorWallOption
    , top : DoorWallOption
    , right : DoorWallOption
    , bottom : DoorWallOption
    }


defaultDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultDoorInfo dorientation =
    { isOpen = False, color = Nothing, orientation = dorientation, requiresToOpen = [], isExplored = False, visibility = Tile.Unexplored }


defaultOpenDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultOpenDoorInfo dorientation =
    { isOpen = True, color = Nothing, orientation = dorientation, requiresToOpen = [], isExplored = False, visibility = Tile.Unexplored }


defaultBlueDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultBlueDoorInfo dorientation =
    { isOpen = False, color = Just "blue", orientation = dorientation, requiresToOpen = [ Key { keyColor = "blue" } ], isExplored = False, visibility = Tile.Unexplored }


defaultBlackDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultBlackDoorInfo dorientation =
    { isOpen = False, color = Just "black", orientation = dorientation, requiresToOpen = [ Key { keyColor = "black" } ], isExplored = False, visibility = Tile.Unexplored }


defaultRedDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultRedDoorInfo dorientation =
    { isOpen = False, color = Just "red", orientation = dorientation, requiresToOpen = [ Key { keyColor = "red" } ], isExplored = False, visibility = Tile.Unexplored }


defaultYellowDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultYellowDoorInfo dorientation =
    { isOpen = False, color = Just "yellow", orientation = dorientation, requiresToOpen = [ Key { keyColor = "yellow" } ], isExplored = False, visibility = Tile.Unexplored }


defaultGreenDoorInfo : Tile.DoorOrientation -> Tile.DoorInfo
defaultGreenDoorInfo dorientation =
    { isOpen = False, color = Just "green", orientation = dorientation, requiresToOpen = [ Key { keyColor = "green" } ], isExplored = False, visibility = Tile.Unexplored }


defaultLeverInfo : Tile.LeverId -> Tile.LeverInfo
defaultLeverInfo lever_id =
    { leverId = lever_id, isUp = False, isTransparent = False, isExplored = False, visibility = Tile.Unexplored }


defaultFlagInfo : Tile.FlagInfo
defaultFlagInfo =
    { condition = Tile.Fine, isTransparent = False, isExplored = False, visibility = Tile.Unexplored }


defaultColumnInfo : Tile.ColumnInfo
defaultColumnInfo =
    { isExplored = False, visibility = Tile.Unexplored }


defaultWaterWallUpInfo : Tile.WaterInfo
defaultWaterWallUpInfo =
    { description = "water_wall_up", isTransparent = False, isWalkable = False, isExplored = False, visibility = Tile.Unexplored }


defaultWaterWallLeftInfo : Tile.WaterInfo
defaultWaterWallLeftInfo =
    { description = "water_wall_left", isTransparent = False, isWalkable = False, isExplored = False, visibility = Tile.Unexplored }


defaultWaterInfo : Tile.WaterInfo
defaultWaterInfo =
    { description = "just_water", isTransparent = False, isWalkable = False, isExplored = False, visibility = Tile.Unexplored }


walkableWaterInfo : Tile.WaterInfo
walkableWaterInfo =
    { description = "just_water", isTransparent = False, isWalkable = True, isExplored = False, visibility = Tile.Unexplored }


type Input
    = Up
    | Down
    | Left
    | Right
    | PickUpItem
    | ViewInventory
    | ViewStatsOverlay
    | ViewOpponentReport
    | ViewHelpMode
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


isTree : Tile -> Bool
isTree tile =
    case tile of
        Tree _ ->
            True

        _ ->
            False


isConverterTile : Tile -> Bool
isConverterTile tile =
    case tile of
        ConverterTile _ _ ->
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



{- }
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
-}


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


isTileTransparent : Tile -> Bool
isTileTransparent tile =
    case tile of
        Floor floorinfo ->
            floorinfo.isTransparent

        Grass grassinfo ->
            grassinfo.isTransparent

        Tree treeInfo ->
            False

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

        ConverterTile it ct ->
            False

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

        Tree tinfo ->
            False

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

        ConverterTile it ct ->
            False

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

        Tree treeinfo ->
            Tree { treeinfo | isExplored = True }

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

        ConverterTile it ct ->
            ConverterTile it ct

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


getTileVisibility : Tile -> Tile.Visibility
getTileVisibility tile =
    case tile of
        Floor floorinfo ->
            floorinfo.visibility

        Grass grassinfo ->
            grassinfo.visibility

        Tree treeinfo ->
            treeinfo.visibility

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

        ConverterTile it ct ->
            Tile.Visible

        NoTileYet ->
            Tile.Unexplored


getGridTileVisibility : Location -> Grid.Grid Tile -> Tile.Visibility
getGridTileVisibility location_ gridtiles =
    Grid.get location_ gridtiles
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Tile.Unexplored


getModelTileVisibility : Location -> Model -> Tile.Visibility
getModelTileVisibility location_ model =
    Grid.get location_ model.level
        |> Maybe.map getTileVisibility
        |> Maybe.withDefault Tile.Unexplored


setTileVisibility : Tile.Visibility -> Tile -> Tile
setTileVisibility visibility_ tile =
    case tile of
        Floor floorinfo ->
            Floor { floorinfo | visibility = visibility_ }

        Grass grassinfo ->
            Grass { grassinfo | visibility = visibility_ }

        Tree treeInfo ->
            Tree { treeInfo | visibility = visibility_ }

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

        ConverterTile it ct ->
            ConverterTile it ct

        NoTileYet ->
            NoTileYet


setModelTileVisibility : Location -> Tile.Visibility -> Model -> Model
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

                Tree treeinfo ->
                    "t"

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

                ConverterTile it ct ->
                    "c"

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


visibility : Model -> Location -> Tile.Visibility
visibility model location_ =
    --Grid.getWithDefault Tile.Unexplored location model.explored
    getModelTileVisibility location_ model


inList : a -> List a -> Bool
inList a_val la =
    List.filter (\elem -> elem == a_val) la
        |> List.length
        |> (\x -> x > 0)
