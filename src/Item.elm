module Item exposing
    ( Item(..)
    , KeyInfo
    , PaperInfo
    , itemToImgSrc
    , itemToString
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
    , imgSrc : String
    , description : String
    , text : String
    }


itemToImgSrc : Item -> String
itemToImgSrc item =
    case item of
        Key keyInfo ->
            "img/items/key_" ++ keyInfo.keyColor ++ "_inventory.png"

        Paper paperInfo ->
            "img/items/paper_part" ++ String.fromInt paperInfo.id ++ ".png"

        _ ->
            -- not important for now
            ""


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
