module Tile exposing
    ( ColumnInfo
    , DoorInfo
    , DoorOrientation(..)
    , DoorWallOption(..)
    , DoorWallOptions
    , FlagCondition(..)
    , FlagInfo
    , FloorDrawing(..)
    , FloorInfo
    , GrassInfo
    , HoleInfo
    , LeverId
    , LeverInfo
    , RoomType(..)
    , StairsInfo
    , TeleporterInfo
    , TeleporterType(..)
    , Tile(..)
    , TreeInfo
    , Visibility(..)
    , WallInfo
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
    , defaultPineTreeInfo
    , defaultRedDoorInfo
    , defaultRoundTreeInfo
    , defaultWallInfo
    , defaultWallUpInfo
    , defaultWaterInfo
    , defaultWaterWallLeftInfo
    , defaultWaterWallUpInfo
    , defaultYellowDoorInfo
    , getTileVisibility
    , isConverterTile
    , isFloor
    , isGrass
    , isHole
    , isMbTileWall
    , isNoTileYet
    , isStairs
    , isTileExplored
    , isTileTransparent
    , isTree
    , isWall
    , setTileAsExplored
    , setTileVisibility
    , setWallTileOrientation
    , walkableWaterInfo
    )

import Item exposing (Item(..))


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
    | Tree TreeInfo
    | ConverterTile Tile Tile
    | NoTileYet


type alias TreeInfo =
    { treeType : String
    , isExplored : Bool
    , visibility : Visibility
    }


type alias GrassInfo =
    { description : String
    , isTransparent : Bool
    , isWalkable : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


type alias FloorInfo =
    { item : Maybe Item
    , floorDrawing : Maybe FloorDrawing
    , isTransparent : Bool
    , isWalkable : Bool
    , isExplored : Bool
    , visibility : Visibility
    , color : String
    }


type FloorDrawing
    = LandingTargetDrawing Int


type alias WallInfo =
    { isExplored : Bool
    , visibility : Visibility
    , orientation : String
    , mbTeleporterObject : Maybe TeleporterInfo
    }


type alias DoorInfo =
    { isOpen : Bool
    , color : Maybe String
    , orientation : DoorOrientation
    , requiresToOpen : List Item
    , isExplored : Bool
    , visibility : Visibility
    }


type DoorOrientation
    = DoorToTheRight
    | DoorToTheLeft
    | DoorToUp
    | DoorToDown


type alias LeverId =
    Int


type alias LeverInfo =
    { leverId : LeverId
    , isUp : Bool
    , isTransparent : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


type alias FlagInfo =
    { condition : FlagCondition
    , isTransparent : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


type FlagCondition
    = Ruined
    | Fine


type alias ColumnInfo =
    { isExplored : Bool
    , visibility : Visibility
    }


type Visibility
    = Visible
    | Unexplored
    | Explored


type alias WaterInfo =
    { description : String
    , isTransparent : Bool
    , isWalkable : Bool
    , isExplored : Bool
    , visibility : Visibility
    }


type WallJunction
    = Flat
    | Empty


type alias WallOverInfo =
    { r : WallJunction, l : WallJunction, u : WallJunction, d : WallJunction, isExplored : Bool, visibility : Visibility }


type alias StairsInfo =
    { stairsId : Int
    , toFloorId : Int
    , toStairsId : Int
    , shift : ( Int, Int ) --after the player has been moved to the destination stairs what's the shift applied relative to the stairs position
    , isExplored : Bool
    , visibility : Visibility
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


type RoomType
    = SquareRoom
    | HorizontalRoom
    | VerticalRoom


defaultFloorInfo : FloorInfo
defaultFloorInfo =
    { item = Nothing, floorDrawing = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored, color = "default" }


defaultOrangeFloorInfo : FloorInfo
defaultOrangeFloorInfo =
    { item = Nothing, floorDrawing = Nothing, isTransparent = True, isWalkable = True, isExplored = False, visibility = Unexplored, color = "orange" }


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


defaultPineTreeInfo : TreeInfo
defaultPineTreeInfo =
    { treeType = "pinetree", isExplored = False, visibility = Unexplored }


defaultRoundTreeInfo : TreeInfo
defaultRoundTreeInfo =
    { treeType = "roundtree", isExplored = False, visibility = Unexplored }


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


defaultDoorInfo : DoorOrientation -> DoorInfo
defaultDoorInfo dorientation =
    { isOpen = False, color = Nothing, orientation = dorientation, requiresToOpen = [], isExplored = False, visibility = Unexplored }


defaultOpenDoorInfo : DoorOrientation -> DoorInfo
defaultOpenDoorInfo dorientation =
    { isOpen = True, color = Nothing, orientation = dorientation, requiresToOpen = [], isExplored = False, visibility = Unexplored }


defaultBlueDoorInfo : DoorOrientation -> DoorInfo
defaultBlueDoorInfo dorientation =
    { isOpen = False, color = Just "blue", orientation = dorientation, requiresToOpen = [ Key { keyColor = "blue" } ], isExplored = False, visibility = Unexplored }


defaultBlackDoorInfo : DoorOrientation -> DoorInfo
defaultBlackDoorInfo dorientation =
    { isOpen = False, color = Just "black", orientation = dorientation, requiresToOpen = [ Key { keyColor = "black" } ], isExplored = False, visibility = Unexplored }


defaultRedDoorInfo : DoorOrientation -> DoorInfo
defaultRedDoorInfo dorientation =
    { isOpen = False, color = Just "red", orientation = dorientation, requiresToOpen = [ Key { keyColor = "red" } ], isExplored = False, visibility = Unexplored }


defaultYellowDoorInfo : DoorOrientation -> DoorInfo
defaultYellowDoorInfo dorientation =
    { isOpen = False, color = Just "yellow", orientation = dorientation, requiresToOpen = [ Key { keyColor = "yellow" } ], isExplored = False, visibility = Unexplored }


defaultGreenDoorInfo : DoorOrientation -> DoorInfo
defaultGreenDoorInfo dorientation =
    { isOpen = False, color = Just "green", orientation = dorientation, requiresToOpen = [ Key { keyColor = "green" } ], isExplored = False, visibility = Unexplored }


defaultLeverInfo : LeverId -> LeverInfo
defaultLeverInfo lever_id =
    { leverId = lever_id, isUp = False, isTransparent = False, isExplored = False, visibility = Unexplored }


defaultFlagInfo : FlagInfo
defaultFlagInfo =
    { condition = Fine, isTransparent = False, isExplored = False, visibility = Unexplored }


defaultColumnInfo : ColumnInfo
defaultColumnInfo =
    { isExplored = False, visibility = Unexplored }


defaultWaterWallUpInfo : WaterInfo
defaultWaterWallUpInfo =
    { description = "water_wall_up", isTransparent = False, isWalkable = False, isExplored = False, visibility = Unexplored }


defaultWaterWallLeftInfo : WaterInfo
defaultWaterWallLeftInfo =
    { description = "water_wall_left", isTransparent = False, isWalkable = False, isExplored = False, visibility = Unexplored }


defaultWaterInfo : WaterInfo
defaultWaterInfo =
    { description = "just_water", isTransparent = False, isWalkable = False, isExplored = False, visibility = Unexplored }


walkableWaterInfo : WaterInfo
walkableWaterInfo =
    { description = "just_water", isTransparent = False, isWalkable = True, isExplored = False, visibility = Unexplored }


getTileVisibility : Tile -> Visibility
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
            Visible

        NoTileYet ->
            Unexplored


setTileVisibility : Visibility -> Tile -> Tile
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



{- }
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

-}
--|> Text.monospace
--Element.centered << Text.monospace << Text.fromString <| c
