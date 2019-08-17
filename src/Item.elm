module Item exposing
    ( Item(..)
    , KeyInfo
    , PaperInfo
    )


type alias Size =
    Int


type alias FoodDescription =
    String


type Item
    = Chest Size
    | Skull
    | Key KeyInfo
    | Money
    | Box
    | Ash
    | Paper PaperInfo
    | Food FoodDescription


type alias KeyInfo =
    { keyColor : String
    }


type alias PaperInfo =
    { id : Int
    , imageStr : String
    , description : String
    , text : String
    }
