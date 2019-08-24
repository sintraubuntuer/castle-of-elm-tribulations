module Tile exposing
    ( ColumnInfo
    , DoorInfo
    , DoorOrientation(..)
    , FlagCondition(..)
    , FlagInfo
    , FloorDrawing(..)
    , FloorInfo
    , GrassInfo
    , HoleInfo
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


type alias LeverInfo =
    { isUp : Bool
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
