module Item exposing
    ( Item(..)
    , KeyInfo
    )


type alias Size =
    Int


type Item
    = Chest Size
    | Skull
    | Key KeyInfo
    | Money
    | Box
    | Ash


type alias KeyInfo =
    { keyColor : String
    }
