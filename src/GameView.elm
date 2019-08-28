module GameView exposing (view)

import Beings.Beings as Beings exposing (FightingCharacter, FightingCharacterId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Collage exposing (..)
import Collage.Events
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text as Text
import Color exposing (..)
import Debug
import Dict exposing (Dict)
import GameModel exposing (Model)
import GameUpdate exposing (Msg(..))
import Grid
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Item exposing (Item(..), KeyInfo)
import Thorns.Types
import Thorns.View as ThornsView
import Tile exposing (Tile(..), Visibility(..))


xScale : Int
xScale =
    64


yScale : Int
yScale =
    64


noForm : Collage Msg
noForm =
    Collage.circle 0 |> filled (uniform red)


floor_ :
    Tile.FloorInfo
    -> Collage Msg
floor_ floorinfo =
    let
        fileStr =
            if floorinfo.color == "orange" then
                "img/floor/floor_01.png"

            else
                "img/floor/floor_01.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


floor : Tile.FloorInfo -> Collage Msg
floor floorinfo =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform blue)


floorOverlay : String -> Collage Msg
floorOverlay elemStr =
    if elemStr == "ash" then
        Collage.image ( toFloat (xScale - 30), toFloat (yScale - 30) ) "img/floor/floor_ash.png"

    else if String.startsWith "key" elemStr then
        Collage.image ( toFloat (xScale - 30), toFloat (yScale - 30) ) ("img/items/" ++ elemStr ++ ".png")

    else if elemStr == "landingTarget" then
        Collage.image ( toFloat (xScale - 30), toFloat (yScale - 30) ) "img/floor/floor_landing_target.png"

    else
        noForm


stairs : String -> Collage Msg
stairs upOrDownStr =
    let
        fileStr =
            if String.toLower upOrDownStr == "up" then
                "img/floor/floor_stairs_up.png"

            else
                "img/floor/floor_stairs_down.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


hole : Collage Msg
hole =
    let
        fileStr =
            "img/floor/floor_hole.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


wall : String -> Collage Msg
wall orientationStr =
    let
        fileStr =
            if String.toLower orientationStr == "four_way" then
                "img/walls/empty-empty-empty-empty.png"

            else if String.toLower orientationStr == "three_way_at_bottom" then
                "img/walls/empty-empty-empty-flat.png"

            else if String.toLower orientationStr == "three_way_at_right" then
                "img/walls/empty-empty-flat-empty.png"

            else if String.toLower orientationStr == "three_way_at_top" then
                "img/walls/empty-flat-empty-empty.png"

            else if String.toLower orientationStr == "three_way_at_left" then
                "img/walls/flat-empty-empty-empty.png"

            else if String.toLower orientationStr == "corner_top_right" then
                "img/walls/empty-flat-flat-empty.png"

            else if String.toLower orientationStr == "corner_top_left" then
                "img/walls/flat-flat-empty-empty.png"

            else if String.toLower orientationStr == "corner_bottom_right" then
                "img/walls/empty-empty-flat-flat.png"

            else if String.toLower orientationStr == "corner_bottom_left" then
                "img/walls/flat-empty-empty-flat.png"

            else if String.toLower orientationStr == "up" then
                "img/walls/flat-empty-flat-empty.png"

            else if String.toLower orientationStr == "horizontal" then
                "img/walls/empty-flat-empty-flat.png"

            else if String.toLower orientationStr == "cul_de_sac_at_bottom" then
                "img/walls/flat-empty-flat-flat.png"

            else if String.toLower orientationStr == "cul_de_sac_at_top" then
                "img/walls/flat-flat-flat-empty.png"

            else if String.toLower orientationStr == "cul_de_sac_at_left" then
                "img/walls/flat-flat-empty-flat.png"

            else if String.toLower orientationStr == "cul_de_sac_at_right" then
                "img/walls/empty-flat-flat-flat.png"

            else if String.toLower orientationStr == "just_bricks" then
                "img/walls/wall.png"

            else
                "img/walls/wall.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


wallOverlay : Tile.WallInfo -> Collage Msg
wallOverlay wallinfo =
    let
        woverlay =
            case wallinfo.mbTeleporterObject of
                Just tinfo ->
                    case tinfo.teleporterType of
                        Tile.Barrel ->
                            Collage.image ( toFloat xScale, toFloat yScale ) "img/walls/wall_overlay_teleporter_barrel_up.png"

                        Tile.BookCase ->
                            Collage.image ( toFloat xScale, toFloat yScale ) "img/walls/wall_overlay_teleporter_bookcase_up.png"

                        Tile.Clock ->
                            Collage.image ( toFloat xScale, toFloat yScale ) "img/walls/wall_overlay_teleporter_clock_up.png"

                _ ->
                    noForm
    in
    woverlay


door : Tile.DoorInfo -> Collage Msg
door doorinfo =
    let
        mbFileStr =
            if doorinfo.isOpen && (doorinfo.orientation == Tile.DoorToUp || doorinfo.orientation == Tile.DoorToDown) then
                Just "img/doors/doorUp_open_floorBg.png"

            else if doorinfo.isOpen && doorinfo.orientation == Tile.DoorToTheLeft then
                Just "img/doors/doorLeft_open_floorBg.png"

            else if doorinfo.isOpen && doorinfo.orientation == Tile.DoorToTheRight then
                Just "img/doors/doorRight_open_floorBg.png"

            else if doorinfo.isOpen then
                Just "img/floor/floor_01.png"

            else if doorinfo.color == Just "blue" then
                Just "img/doors/blueDoorClosed_floorBg.png"

            else if doorinfo.color == Just "green" then
                Just "img/doors/greenDoorClosed_floorBg.png"

            else if doorinfo.color == Just "red" then
                Just "img/doors/redDoorClosed_floorBg.png"

            else if doorinfo.color == Just "yellow" then
                Just "img/doors/yellowDoorClosed_floorBg.png"

            else if doorinfo.color == Just "black" then
                Just "img/doors/blackDoorClosed_floorBg.png"

            else
                Just "img/doors/doorClosed_floorBg.png"
    in
    case mbFileStr of
        Just fileStr ->
            Collage.image ( toFloat xScale, toFloat yScale ) fileStr

        Nothing ->
            noForm


stringToColor : String -> Color
stringToColor theColorStr =
    if String.toLower theColorStr == "blue" then
        blue

    else if String.toLower theColorStr == "white" then
        white

    else if String.toLower theColorStr == "red" then
        red

    else if String.toLower theColorStr == "yellow" then
        yellow

    else if String.toLower theColorStr == "green" then
        green

    else
        white


doorOverlay : Tile.DoorInfo -> Collage Msg
doorOverlay doorInfo =
    noForm


lever : String -> Collage Msg
lever onOffStr =
    let
        fileStr =
            if onOffStr == "on" || onOffStr == "On" || onOffStr == "ON" then
                "img/levers/lever_color_on.png"

            else
                "img/levers/lever_color_off.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


acid : Collage Msg
acid =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform darkGreen)


acidOverlay : Collage Msg
acidOverlay =
    noForm


notileyet : Collage Msg
notileyet =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform black)


notileyetOverlay : Collage Msg
notileyetOverlay =
    noForm


fog : Collage Msg
fog =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform (rgba 0 0 0 1))


halfFog : Collage Msg
halfFog =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform (rgba 0 0 0 0.6))


tile : Int -> Tile -> Collage Msg
tile currentFloorId t =
    case t of
        Tile.Floor floorinfo ->
            floor_ floorinfo

        Tile.Stairs sinfo ->
            if sinfo.toFloorId > currentFloorId then
                stairs "up"

            else
                stairs "down"

        Tile.Tree treeinfo ->
            tree treeinfo

        Tile.Hole hinfo ->
            hole

        Tile.Wall wallinfo ->
            if wallinfo.orientation == "four_way" then
                wall "four_way"

            else if wallinfo.orientation == "three_way_at_bottom" then
                wall "three_way_at_bottom"

            else if wallinfo.orientation == "three_way_at_right" then
                wall "three_way_at_right"

            else if wallinfo.orientation == "three_way_at_top" then
                wall "three_way_at_top"

            else if wallinfo.orientation == "three_way_at_left" then
                wall "three_way_at_left"

            else if wallinfo.orientation == "corner_top_right" then
                wall "corner_top_right"

            else if wallinfo.orientation == "corner_top_left" then
                wall "corner_top_left"

            else if wallinfo.orientation == "corner_bottom_right" then
                wall "corner_bottom_right"

            else if wallinfo.orientation == "corner_bottom_left" then
                wall "corner_bottom_left"

            else if wallinfo.orientation == "up" then
                wall "up"

            else if wallinfo.orientation == "horizontal" then
                wall "horizontal"

            else if wallinfo.orientation == "cul_de_sac_at_bottom" then
                wall "cul_de_sac_at_bottom"

            else if wallinfo.orientation == "cul_de_sac_at_top" then
                wall "cul_de_sac_at_top"

            else if wallinfo.orientation == "cul_de_sac_at_left" then
                wall "cul_de_sac_at_left"

            else if wallinfo.orientation == "cul_de_sac_at_right" then
                wall "cul_de_sac_at_right"

            else if wallinfo.orientation == "just_bricks" then
                wall "just_bricks"

            else
                wall "horizontal"

        Tile.Door doorinfo ->
            door doorinfo

        Tile.NoTileYet ->
            notileyet

        Tile.Lever leverinfo ->
            if leverinfo.isUp then
                lever "on"

            else
                lever "off"

        Tile.Water waterinfo ->
            water waterinfo

        Tile.Grass grassinfo ->
            grass grassinfo

        Tile.ConverterTile it ct ->
            tile currentFloorId it

        _ ->
            notileyet


grass : Tile.GrassInfo -> Collage Msg
grass grassinfo =
    let
        fileStr =
            if grassinfo.description == "grass_with_dirt" then
                "img/grass/grass_and_dirt.png"

            else
                "img/grass/grass.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


tree : Tile.TreeInfo -> Collage Msg
tree treeinfo =
    let
        fileStr =
            if treeinfo.treeType == "pinetree" then
                "img/trees/treetopPineTree_bg.png"

            else
                "img/trees/treetopRoundTree_bg.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


water : Tile.WaterInfo -> Collage Msg
water waterinfo =
    let
        fileStr =
            if waterinfo.description == "water_wall_up" then
                "img/water/water_wall_up.png"

            else if waterinfo.description == "water_wall_left" then
                "img/water/water_wall_left.png"

            else
                "img/water/just_water.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


tileOverlay : Tile -> Collage Msg
tileOverlay t =
    case t of
        Tile.Floor floorinfo ->
            case floorinfo.item of
                Just Ash ->
                    floorOverlay "ash"

                Just (Key keyinfo) ->
                    floorOverlay ("key_" ++ keyinfo.keyColor)

                Just Box ->
                    Collage.image ( toFloat xScale, toFloat yScale ) "img/items/box.png"

                Just (Paper paperinfo) ->
                    if paperinfo.id == 1 then
                        Collage.image ( toFloat xScale, toFloat yScale ) "img/items/paper_part1.png"

                    else if paperinfo.id == 2 then
                        Collage.image ( toFloat xScale, toFloat yScale ) "img/items/paper_part2.png"

                    else
                        Collage.image ( toFloat xScale, toFloat yScale ) "img/items/paper_part3.png"

                Just (Food fdescription) ->
                    if String.toLower fdescription == "bread" then
                        Collage.image ( toFloat xScale, toFloat yScale ) "img/items/health_item.png"

                    else
                        Collage.image ( toFloat xScale, toFloat yScale ) "img/items/box.png"

                _ ->
                    case floorinfo.floorDrawing of
                        Just (Tile.LandingTargetDrawing nr) ->
                            floorOverlay "landingTarget"

                        _ ->
                            floorOverlay ""

        Tile.Wall wallinfo ->
            wallOverlay wallinfo

        Tile.Door doorinfo ->
            doorOverlay doorinfo

        Tile.ConverterTile it ct ->
            tileOverlay it

        Tile.NoTileYet ->
            notileyetOverlay

        _ ->
            notileyetOverlay


fogT : Tile.Visibility -> Collage Msg
fogT visibility =
    case visibility of
        Visible ->
            noForm

        Explored ->
            halfFog

        Unexplored ->
            fog


player : Collage Msg
player =
    Collage.circle (toFloat xScale / 2) |> filled (uniform red)


fightingCharacter : Collage Msg
fightingCharacter =
    Collage.circle (toFloat xScale / 2) |> filled (uniform green)


tCollageFromStr : String -> Collage Msg
tCollageFromStr elemStr =
    elemStr
        |> Text.fromString
        |> Text.color black
        |> Collage.rendered


playerImg : Player -> Tile.Visibility -> Collage Msg
playerImg player_ visibility =
    case visibility of
        Tile.Visible ->
            let
                fileStr =
                    case player_.direction of
                        Beings.Left ->
                            "img/pc/left.png"

                        Beings.Right ->
                            "img/pc/right.png"

                        Beings.Up ->
                            "img/pc/up.png"

                        Beings.Down ->
                            "img/pc/down.png"
            in
            Collage.image ( toFloat xScale, toFloat yScale ) fileStr

        _ ->
            noForm


fightingCharacterView : Beings.FightingCharacter -> Bool -> Tile.Visibility -> Collage Msg
fightingCharacterView fightChar showBlood visibility =
    case visibility of
        Tile.Visible ->
            let
                fileStr =
                    if fightChar.indexOfLight >= fightChar.indexOfLightMax then
                        "img/characters/" ++ String.toLower fightChar.species ++ "_enlightened.png"

                    else if fightChar.health > 0 then
                        "img/characters/" ++ String.toLower fightChar.species ++ ".png"

                    else if fightChar.health <= 0 && showBlood then
                        "img/characters/" ++ String.toLower fightChar.species ++ "_dead_blood.png"

                    else
                        "img/characters/" ++ String.toLower fightChar.species ++ "_dead.png"
            in
            Collage.image ( toFloat xScale, toFloat yScale ) fileStr

        _ ->
            noForm


otherCharacterView : Beings.OtherCharacter -> Bool -> Tile.Visibility -> Collage Msg
otherCharacterView character showBlood visibility =
    case visibility of
        Tile.Visible ->
            let
                fileStr =
                    "img/pc/right.png"

                {-
                     if character.indexOfLight >= character.indexOfLightMax then
                         "img/characters/" ++ String.toLower fightChar.species ++ "_enlightened.png"

                     else if character.health > 0 then
                         "img/characters/" ++ String.toLower fightChar.species ++ ".png"

                     else if character.health <= 0 && showBlood then
                         "img/characters/" ++ String.toLower fightChar.species ++ "_dead_blood.png"

                     else
                         "img/characters/" ++ String.toLower fightChar.species ++ "_dead.png"
                   -
                -}
            in
            Collage.image ( toFloat xScale, toFloat yScale ) fileStr

        _ ->
            noForm


guy : { r | textAvatar : String } -> Tile.Visibility -> Collage Msg
guy r visibility =
    case visibility of
        Tile.Visible ->
            let
                form =
                    r.textAvatar |> tCollageFromStr

                ( xSize, ySize ) =
                    ( 16, 16 )

                divInts x y =
                    toFloat x / toFloat y

                factor =
                    min (divInts xScale xSize) (divInts yScale ySize)
            in
            scale factor form

        _ ->
            noForm


mainScreen : Model -> Collage Msg
mainScreen model =
    let
        ( subgrid, txtmsg ) =
            model.level
                |> Grid.getSubGrid model.x_display_anchor (model.x_display_anchor + model.window_width - 1) model.y_display_anchor (model.y_display_anchor + model.window_height - 1)

        ( wwidth, wheight ) =
            ( subgrid.size.width, subgrid.size.height )

        ( w, h ) =
            ( wwidth * xScale, wheight * yScale )

        xOffset : Int -> Float
        xOffset n =
            (toFloat n - toFloat model.x_display_anchor - toFloat wwidth / 2) * toFloat xScale

        yOffset : Int -> Float
        yOffset n =
            (toFloat n - toFloat model.y_display_anchor - toFloat wheight / 2) * toFloat yScale

        xOffset_for_subgrid : Int -> Float
        xOffset_for_subgrid n =
            (toFloat n - toFloat wwidth / 2) * toFloat xScale

        yOffset_for_subgrid : Int -> Float
        yOffset_for_subgrid n =
            (toFloat n - toFloat wheight / 2) * toFloat yScale

        location : { r | location : GameModel.Location } -> ( Float, Float )
        location r =
            ( xOffset r.location.x, 0 - yOffset (r.location.y + 1) )

        mkLayer : List (List a) -> (( Int, List a ) -> List (Collage Msg)) -> Collage Msg
        mkLayer agrid mapRow =
            let
                rows =
                    List.map2 (\v1 v2 -> ( v1, v2 )) (List.reverse (List.range 0 (wheight - 1))) agrid

                forms =
                    List.concatMap mapRow rows
            in
            Collage.group forms

        row : (a -> Collage Msg) -> ( Int, List a ) -> List (Collage Msg)
        row mkTile ( n, tiles ) =
            let
                tiles_ =
                    List.map2 (\v1 v2 -> ( v1, v2 )) (List.range 0 (wwidth - 1)) tiles

                makeTile ( n_, t ) =
                    shift ( xOffset_for_subgrid n_, yOffset_for_subgrid n ) <| mkTile t
            in
            List.map makeTile tiles_

        player_ =
            playerImg model.player Tile.Visible
                |> shift (location model.player)

        fightingCharacter_ =
            let
                relevantFightingCharactersDict =
                    Dict.filter (\fcharId fightChar -> (fightChar.floorId == model.currentFloorId) && (fightChar.location.x >= model.x_display_anchor && fightChar.location.x - model.x_display_anchor < model.window_width) && (fightChar.location.y >= model.y_display_anchor && fightChar.location.y - model.y_display_anchor < model.window_height)) model.fightingCharacters

                mkfightingCharacter fcharId anfightingCharacter =
                    fightingCharacterView anfightingCharacter model.showBlood (GameModel.getGridTileVisibility anfightingCharacter.location model.level)
                        |> shift (location anfightingCharacter)
            in
            group <| (Dict.map mkfightingCharacter relevantFightingCharactersDict |> Dict.values)

        otherCharacters_ =
            let
                relevantOtherCharsDict =
                    Dict.filter (\charId char -> (char.floorId == model.currentFloorId) && (char.location.x >= model.x_display_anchor && char.location.x - model.x_display_anchor < model.window_width) && (char.location.y >= model.y_display_anchor && char.location.y - model.y_display_anchor < model.window_height)) model.otherCharacters

                mkOtherChar ch_id achar =
                    otherCharacterView achar model.showBlood (GameModel.getGridTileVisibility achar.location model.level)
                        |> shift (location achar)
            in
            group <| (Dict.map mkOtherChar relevantOtherCharsDict |> Dict.values)

        bg : Collage Msg
        bg =
            Collage.group
                [ mkLayer (Grid.toList subgrid) (row tileOverlay)
                , mkLayer (Grid.toList subgrid) (row (tile model.currentFloorId))
                ]
                |> name "background"

        pos =
            locate "background" topLeft bg
                |> Maybe.withDefault ( -100000, -100000 )

        pg =
            Collage.group
                [ player_ |> shift ( 0, 0 )
                ]

        eg =
            Collage.group
                [ fightingCharacter_ ]

        ocg =
            Collage.group
                [ otherCharacters_ ]

        emptyg =
            Collage.group
                []

        visibilitySubGrid =
            Grid.map (\t -> Tile.getTileVisibility t) subgrid

        fogger =
            mkLayer (Grid.toList visibilitySubGrid) (row fogT)
    in
    Collage.group
        ([ fogger
         , pg |> shift ( 0, 0 )
         , eg
         , ocg
         , bg
         ]
         --  ++ List.map (Text.fromString >> Collage.rendered) (List.take 3 model.log)
        )
        |> name "mainScreen"


inViewRange : FightingCharacter -> Bool
inViewRange fightingCharacter_ =
    False


sidebar : Model -> ( Float, Float ) -> Collage Msg
sidebar model pos =
    let
        x =
            5

        theColor =
            Text.color white

        theColor2 =
            Text.color red

        bar =
            [ model.player.textAvatar ++ " : " ++ model.player.name |> Text.fromString |> theColor |> Collage.rendered
            , "Health: " ++ String.fromInt model.player.health |> Text.fromString |> theColor |> Collage.rendered
            , "mana: " ++ String.fromInt model.player.mana |> Text.fromString |> theColor |> Collage.rendered
            ]
                |> List.indexedMap (\i elem -> shift ( -100, 200 - toFloat i * 25 ) elem)
                |> Collage.group

        barDebugMode =
            [ model.player.textAvatar ++ " : " ++ model.player.name |> Text.fromString |> theColor |> Collage.rendered
            , "Health: " ++ String.fromInt model.player.health |> Text.fromString |> theColor |> Collage.rendered
            , "Energy: " ++ String.fromInt model.player.energy |> Text.fromString |> theColor |> Collage.rendered
            , "mana: " ++ String.fromInt model.player.mana |> Text.fromString |> theColor |> Collage.rendered
            , "Hunger: " ++ String.fromInt model.player.hunger |> Text.fromString |> theColor |> Collage.rendered
            , "Stealth: " ++ String.fromInt model.player.stealth ++ "%" |> Text.fromString |> theColor |> Collage.rendered
            , "Armor: " ++ String.fromInt model.player.armor |> Text.fromString |> theColor |> Collage.rendered
            , "Protection: " ++ String.fromInt model.player.protection ++ "%" |> Text.fromString |> theColor |> Collage.rendered
            , "Coordination: " ++ String.fromInt model.player.coordination ++ "%" |> Text.fromString |> theColor |> Collage.rendered
            , "Power: " ++ String.fromInt model.player.power |> Text.fromString |> theColor |> Collage.rendered
            , "Initiative: " ++ String.fromInt model.player.initiative |> Text.fromString |> theColor |> Collage.rendered
            , "x_display_anchor: " ++ String.fromInt model.x_display_anchor |> Text.fromString |> theColor |> Collage.rendered
            , "y_display_anchor: " ++ String.fromInt model.y_display_anchor |> Text.fromString |> theColor |> Collage.rendered
            , "current_player_x : " ++ String.fromInt model.player.location.x |> Text.fromString |> theColor |> Collage.rendered
            , "current_player_y : " ++ String.fromInt model.player.location.y |> Text.fromString |> theColor |> Collage.rendered
            , "wall percentage : " ++ String.fromFloat (model.wallPercentage |> Maybe.withDefault 0) |> Text.fromString |> theColor |> Collage.rendered
            ]
                |> List.indexedMap (\i elem -> shift ( -100, 200 - toFloat i * 25 ) elem)
                |> Collage.group
    in
    if model.debugMode then
        barDebugMode

    else
        bar


viewGameOverOverlay : Bool -> Collage Msg
viewGameOverOverlay completed =
    let
        theColor =
            Text.color white

        theColor2 =
            Text.color red

        completionMsg =
            if completed then
                "You have completed all your quests and have become an enlightened version of yourself "

            else
                ""
    in
    [ "GAME OVER" |> Text.fromString |> theColor |> Collage.rendered
    , completionMsg |> Text.fromString |> theColor |> Collage.rendered
    ]
        |> List.indexedMap (\i elem -> shift ( -100, 200 - toFloat i * 25 ) elem)
        |> Collage.group


viewItem : Item.Item -> Collage Msg
viewItem item =
    case item of
        Key { keyColor } ->
            Collage.image ( toFloat xScale, toFloat yScale ) ("img/items/key_" ++ String.toLower keyColor ++ ".png")

        Box ->
            Collage.image ( toFloat xScale, toFloat yScale ) "img/items/box.png"

        Paper paperinfo ->
            if paperinfo.id == 1 then
                Collage.image ( toFloat xScale, toFloat yScale ) "img/items/paper_part1.png"

            else if paperinfo.id == 2 then
                Collage.image ( toFloat xScale, toFloat yScale ) "img/items/paper_part2.png"

            else
                Collage.image ( toFloat xScale, toFloat yScale ) "img/items/paper_part3.png"

        _ ->
            noForm


viewInventoryOverlay : Model -> Collage Msg
viewInventoryOverlay model =
    let
        theColor =
            Text.color white

        top =
            [ "You have currently in your inventory : " |> Text.fromString |> theColor |> Collage.rendered |> shift ( 0, 70 ) ]

        forms =
            List.map (\it -> viewItem it) (Dict.values model.player.inventory)

        topAndForms =
            List.indexedMap (\i item -> item |> shift ( 0, 1 * (0 + toFloat i * 50) )) (top ++ forms)
    in
    Collage.group topAndForms


viewInventory : Model -> Html Msg
viewInventory model =
    let
        theColor =
            Text.color white

        top =
            Html.h3 [] [ Html.text "Inventory : " ]

        thelist =
            Html.div []
                (List.concatMap (\it -> [ Html.a [] [ Html.img [ Attr.size 640, Attr.src (Item.itemToImgSrc it) ] [] ], Html.br [] [] ]) (Dict.values model.player.inventory))
    in
    Html.div [ Attr.align "center" ]
        [ top
        , thelist
        , Html.br [] []
        , Html.br [] []
        , Html.text "Press I to leave Inventory"
        ]


display :
    Model
    -> Collage Msg
display model =
    let
        pos =
            locate "mainScreen" topLeft (mainScreen model)
                |> Maybe.withDefault ( -100000, -100000 )
    in
    Collage.group
        [ if model.displayStatsOverlay then
            sidebar model pos

          else
            noForm
        , if model.currentDisplay == GameModel.DisplayGameOver then
            viewGameOverOverlay False

          else if model.currentDisplay == GameModel.DisplayGameCompleted then
            viewGameOverOverlay True

          else
            noForm
        , mainScreen model
        ]


gridToHtmlList : Grid.Grid a -> List (Html Msg)
gridToHtmlList grid =
    let
        lofls =
            Grid.toList grid

        funcListToString l =
            List.foldl (\x y -> y ++ " , " ++ "Todo ... convert grid element to string ") "" l

        lofstrs =
            List.map (\x -> funcListToString x) lofls
    in
    List.map (\astr -> Html.h1 [] [ Html.text astr ]) lofstrs


viewDebugGrid : Grid.Grid a -> Model -> List (Html Msg)
viewDebugGrid grid model =
    let
        ( subgrid, txtmsg ) =
            grid
                |> Grid.getSubGrid model.x_display_anchor (model.x_display_anchor + model.window_width - 1) model.y_display_anchor (model.y_display_anchor + model.window_height - 1)
    in
    [ Html.div []
        ([ Html.h1 [] [ Html.text ("viewDebugGrid has been called with : " ++ txtmsg) ] ]
            ++ (subgrid
                    |> gridToHtmlList
               )
        )
    ]


viewDebugPlayer : Model -> Html Msg
viewDebugPlayer model =
    Html.h2 []
        [ Html.text
            ("player is in position "
                ++ String.fromInt model.player.location.x
                ++ " , "
                ++ String.fromInt model.player.location.y
                ++ " , has health = "
                ++ String.fromInt model.player.health
                ++ " , and energy = "
                ++ String.fromInt model.player.energy
            )
        ]


viewDebugFightingCharacters : Model -> List (Html Msg)
viewDebugFightingCharacters model =
    let
        fightingCharacterToHtmlFunc fightingCharacter_ fightingCharacterId =
            Html.h2 []
                [ Html.text
                    ("fightingCharacter "
                        ++ String.fromInt fightingCharacterId
                        ++ " is in position "
                        ++ String.fromInt fightingCharacter_.location.x
                        ++ " , "
                        ++ String.fromInt fightingCharacter_.location.y
                        ++ " , has health = "
                        ++ String.fromInt fightingCharacter_.health
                    )
                ]
    in
    Dict.map (\fcharId fightingCharacter_ -> fightingCharacterToHtmlFunc fightingCharacter_ fcharId) model.fightingCharacters
        |> Dict.values


viewOpponentReport : Model -> Html GameUpdate.Msg
viewOpponentReport model =
    Html.div [ Attr.align "center" ]
        [ Html.h3 [] [ Html.text "Opponent Report :" ]
        , Html.br [] []
        , Html.div
            []
            (Dict.values model.fightingCharacters |> List.concatMap (\fightChar -> [ Html.text ("name : " ++ fightChar.name ++ " , health : " ++ String.fromInt fightChar.health ++ " ,  IndexOfLight : " ++ String.fromInt fightChar.indexOfLight), Html.br [] [], Html.br [] [], Html.br [] [] ]))
        , Html.br [] []
        , Html.br [] []
        , Html.text "Press E to leave Opponent Report"
        ]


viewHelpMode : Model -> Html GameUpdate.Msg
viewHelpMode model =
    Html.div [ Attr.align "center" ]
        [ Html.h3 [] [ Html.text "Help Screen" ]
        , Html.text "Castle of Elm Tribulations is a minimalistic Rogue like game , inspired by Castle of Elm , Atic Atac , Roguelike in Elm  , Sleeping Beauty's Game of Thorns , ... "
        , Html.br [] []
        , Html.text "Find your way through the Caverns, Basement , The Ground Floor , First Floor and the Attic , pick up the three pieces of paper with the codes to unlock the black door and move towards enlightenment !!!"
        , Html.br [] []
        , Html.br [] []
        , Html.br [] []
        , Html.text "Use Arrow keys to move "
        , Html.br [] []
        , Html.text "U to pick up items"
        , Html.br [] []
        , Html.text "I for inventory"
        , Html.br [] []
        , Html.text "S for Stats"
        , Html.br [] []
        , Html.text "E for Opponent Report "
        , Html.br [] []
        , Html.text "and H for Help"
        , Html.br [] []
        , Html.br [] []
        , Html.text "Press H to leave Help Screen"
        ]


view : Model -> Html GameUpdate.Msg
view model =
    case model.started of
        True ->
            Html.div []
                [ case model.currentDisplay of
                    GameModel.DisplayGameOfThorns ->
                        viewGameOfThorns model

                    GameModel.DisplayOpponentReport ->
                        viewOpponentReport model

                    GameModel.DisplayInventory ->
                        viewInventory model

                    GameModel.DisplayHelpScreen ->
                        viewHelpMode model

                    _ ->
                        Html.div []
                            ([ display model
                                |> svg
                             ]
                             --++ [ viewDebugPlayer model ]
                             --  ++ viewDebugFightingCharacters model
                             --  ++ viewDebugGrid model.level model
                            )
                ]

        False ->
            viewStartMenuChoices model


viewGameOfThorns : Model -> Html GameUpdate.Msg
viewGameOfThorns model =
    Html.div [] [ Html.map GameUpdate.ThornsMsg (ThornsView.view model.gameOfThornsModel) ]


viewStartMenuChoices : Model -> Html GameUpdate.Msg
viewStartMenuChoices model =
    Html.div [ Attr.align "center" ]
        [ Html.div [ Attr.align "center" ]
            [ Html.h3 []
                [ Html.a
                    [ Html.Events.onClick (GameUpdate.StartGameNr 2) ]
                    [ Html.text "Start - Castle of Elm Tribulations" ]
                , Html.br [] []
                , Html.a
                    [ Html.Events.onClick (GameUpdate.StartGameNr 2) ]
                    [ Html.img [ Attr.src "img/game/casteleOfElmTribulations_.png" ] [] ]
                ]
            ]
        , Html.br [] []
        , Html.br [] []
        , Html.div []
            [ Html.h3 []
                [ Html.a [ Html.Events.onClick (GameUpdate.StartGameNr 1) ] [ Html.text "Start Game 1 - Random Dungeon " ]
                ]
            ]
        , Html.br [] []
        ]
