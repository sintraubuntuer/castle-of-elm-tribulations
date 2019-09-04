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
    -> Int
    -> Int
    -> String
    -> Collage Msg
floor_ floorinfo tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if floorinfo.color == "orange" then
                imgBaseDir ++ "/floor/floor_01.png"

            else
                imgBaseDir ++ "/floor/floor_01.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


floor : Tile.FloorInfo -> Int -> Int -> Collage Msg
floor floorinfo tileWidth tileHeight =
    rectangle (toFloat tileWidth) (toFloat tileHeight) |> filled (uniform blue)


floorOverlay : String -> Int -> Int -> String -> Collage Msg
floorOverlay elemStr tileWidth tileHeight imgBaseDir =
    if elemStr == "ash" then
        Collage.image ( toFloat (tileWidth // 2), toFloat (tileHeight // 2) ) (imgBaseDir ++ "/floor/floor_ash.png")

    else if String.startsWith "key" elemStr then
        Collage.image ( toFloat (tileWidth // 2), toFloat (tileHeight // 2) ) (imgBaseDir ++ "/items/" ++ elemStr ++ ".png")

    else if elemStr == "landingTarget" then
        Collage.image ( toFloat (tileWidth // 2), toFloat (tileHeight // 2) ) (imgBaseDir ++ "/floor/floor_landing_target.png")

    else
        noForm


stairs : String -> Int -> Int -> String -> Collage Msg
stairs upOrDownStr tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if String.toLower upOrDownStr == "up" then
                imgBaseDir ++ "/floor/floor_stairs_up.png"

            else
                imgBaseDir ++ "/floor/floor_stairs_down.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


hole : Int -> Int -> String -> Collage Msg
hole tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            imgBaseDir ++ "/floor/floor_hole.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


wall : String -> Int -> Int -> String -> Collage Msg
wall orientationStr tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if String.toLower orientationStr == "four_way" then
                imgBaseDir ++ "/walls/empty-empty-empty-empty.png"

            else if String.toLower orientationStr == "three_way_at_bottom" then
                imgBaseDir ++ "/walls/empty-empty-empty-flat.png"

            else if String.toLower orientationStr == "three_way_at_right" then
                imgBaseDir ++ "/walls/empty-empty-flat-empty.png"

            else if String.toLower orientationStr == "three_way_at_top" then
                imgBaseDir ++ "/walls/empty-flat-empty-empty.png"

            else if String.toLower orientationStr == "three_way_at_left" then
                imgBaseDir ++ "/walls/flat-empty-empty-empty.png"

            else if String.toLower orientationStr == "corner_top_right" then
                imgBaseDir ++ "/walls/empty-flat-flat-empty.png"

            else if String.toLower orientationStr == "corner_top_left" then
                imgBaseDir ++ "/walls/flat-flat-empty-empty.png"

            else if String.toLower orientationStr == "corner_bottom_right" then
                imgBaseDir ++ "/walls/empty-empty-flat-flat.png"

            else if String.toLower orientationStr == "corner_bottom_left" then
                imgBaseDir ++ "/walls/flat-empty-empty-flat.png"

            else if String.toLower orientationStr == "up" then
                imgBaseDir ++ "/walls/flat-empty-flat-empty.png"

            else if String.toLower orientationStr == "horizontal" then
                imgBaseDir ++ "/walls/empty-flat-empty-flat.png"

            else if String.toLower orientationStr == "cul_de_sac_at_bottom" then
                imgBaseDir ++ "/walls/flat-empty-flat-flat.png"

            else if String.toLower orientationStr == "cul_de_sac_at_top" then
                imgBaseDir ++ "/walls/flat-flat-flat-empty.png"

            else if String.toLower orientationStr == "cul_de_sac_at_left" then
                imgBaseDir ++ "/walls/flat-flat-empty-flat.png"

            else if String.toLower orientationStr == "cul_de_sac_at_right" then
                imgBaseDir ++ "/walls/empty-flat-flat-flat.png"

            else if String.toLower orientationStr == "just_bricks" then
                imgBaseDir ++ "/walls/wall.png"

            else
                imgBaseDir ++ "/walls/wall.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


wallOverlay : Tile.WallInfo -> Int -> Int -> String -> Collage Msg
wallOverlay wallinfo tileWidth tileHeight imgBaseDir =
    let
        woverlay =
            case wallinfo.mbTeleporterObject of
                Just tinfo ->
                    case tinfo.teleporterType of
                        Tile.Barrel ->
                            if String.toLower wallinfo.orientation == "up" || String.toLower wallinfo.orientation == "down" then
                                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/walls/wall_overlay_teleporter_barrel_side.png")

                            else
                                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/walls/wall_overlay_teleporter_barrel_up.png")

                        Tile.BookCase ->
                            if String.toLower wallinfo.orientation == "up" || String.toLower wallinfo.orientation == "down" then
                                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/walls/wall_overlay_teleporter_bookcase_side.png")

                            else
                                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/walls/wall_overlay_teleporter_bookcase_up.png")

                        Tile.Clock ->
                            Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/walls/wall_overlay_teleporter_clock_up.png")

                _ ->
                    noForm
    in
    woverlay


door : Tile.DoorInfo -> Int -> Int -> String -> Collage Msg
door doorinfo tileWidth tileHeight imgBaseDir =
    let
        mbFileStr =
            if doorinfo.isOpen && (doorinfo.orientation == Tile.DoorToUp || doorinfo.orientation == Tile.DoorToDown) then
                Just <| imgBaseDir ++ "/doors/doorUp_open_floorBg.png"

            else if doorinfo.isOpen && doorinfo.orientation == Tile.DoorToTheLeft then
                Just <| imgBaseDir ++ "/doors/doorLeft_open_floorBg.png"

            else if doorinfo.isOpen && doorinfo.orientation == Tile.DoorToTheRight then
                Just <| imgBaseDir ++ "/doors/doorRight_open_floorBg.png"

            else if doorinfo.isOpen then
                Just <| imgBaseDir ++ "/floor/floor_01.png"

            else if doorinfo.color == Just "blue" then
                Just <| imgBaseDir ++ "/doors/blueDoorClosed_floorBg.png"

            else if doorinfo.color == Just "green" then
                Just <| imgBaseDir ++ "/doors/greenDoorClosed_floorBg.png"

            else if doorinfo.color == Just "red" then
                Just <| imgBaseDir ++ "/doors/redDoorClosed_floorBg.png"

            else if doorinfo.color == Just "yellow" then
                Just <| imgBaseDir ++ "/doors/yellowDoorClosed_floorBg.png"

            else if doorinfo.color == Just "black" then
                Just <| imgBaseDir ++ "/doors/blackDoorClosed_floorBg.png"

            else if doorinfo.color == Just "striped" then
                Just <| imgBaseDir ++ "/doors/stripedDoorClosed_floorBg.png"

            else
                Just <| imgBaseDir ++ "/doors/doorClosed_floorBg.png"
    in
    case mbFileStr of
        Just fileStr ->
            Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr

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


lever : String -> Int -> Int -> String -> Collage Msg
lever onOffStr tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if onOffStr == "on" || onOffStr == "On" || onOffStr == "ON" then
                imgBaseDir ++ "/levers/lever_color_on.png"

            else
                imgBaseDir ++ "/levers/lever_color_off.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


acid : Int -> Int -> Collage Msg
acid tileWidth tileHeight =
    rectangle (toFloat tileWidth) (toFloat tileHeight) |> filled (uniform darkGreen)


acidOverlay : Collage Msg
acidOverlay =
    noForm


notileyet : Int -> Int -> Collage Msg
notileyet tileWidth tileHeight =
    rectangle (toFloat tileWidth) (toFloat tileHeight) |> filled (uniform black)


notileyetOverlay : Collage Msg
notileyetOverlay =
    noForm


fog : Int -> Int -> Collage Msg
fog tileWidth tileHeight =
    rectangle (toFloat tileWidth) (toFloat tileHeight) |> filled (uniform (rgba 0 0 0 1))


halfFog : Int -> Int -> Collage Msg
halfFog tileWidth tileHeight =
    rectangle (toFloat tileWidth) (toFloat tileHeight) |> filled (uniform (rgba 0 0 0 0.6))


tile : Int -> Int -> Int -> String -> Tile -> Collage Msg
tile currentFloorId tileWidth tileHeight imgBaseDir t =
    case t of
        Tile.Floor floorinfo ->
            floor_ floorinfo tileWidth tileHeight imgBaseDir

        Tile.Stairs sinfo ->
            if sinfo.toFloorId > currentFloorId then
                stairs "up" tileWidth tileHeight imgBaseDir

            else
                stairs "down" tileWidth tileHeight imgBaseDir

        Tile.Tree treeinfo ->
            tree treeinfo tileWidth tileHeight imgBaseDir

        Tile.Hole hinfo ->
            hole tileWidth tileHeight imgBaseDir

        Tile.Wall wallinfo ->
            if wallinfo.orientation == "four_way" then
                wall "four_way" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "three_way_at_bottom" then
                wall "three_way_at_bottom" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "three_way_at_right" then
                wall "three_way_at_right" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "three_way_at_top" then
                wall "three_way_at_top" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "three_way_at_left" then
                wall "three_way_at_left" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "corner_top_right" then
                wall "corner_top_right" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "corner_top_left" then
                wall "corner_top_left" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "corner_bottom_right" then
                wall "corner_bottom_right" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "corner_bottom_left" then
                wall "corner_bottom_left" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "up" then
                wall "up" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "horizontal" then
                wall "horizontal" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "cul_de_sac_at_bottom" then
                wall "cul_de_sac_at_bottom" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "cul_de_sac_at_top" then
                wall "cul_de_sac_at_top" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "cul_de_sac_at_left" then
                wall "cul_de_sac_at_left" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "cul_de_sac_at_right" then
                wall "cul_de_sac_at_right" tileWidth tileHeight imgBaseDir

            else if wallinfo.orientation == "just_bricks" then
                wall "just_bricks" tileWidth tileHeight imgBaseDir

            else
                wall "horizontal" tileWidth tileHeight imgBaseDir

        Tile.Door doorinfo ->
            door doorinfo tileWidth tileHeight imgBaseDir

        Tile.NoTileYet ->
            notileyet tileWidth tileHeight

        Tile.Lever leverinfo ->
            if leverinfo.isUp then
                lever "on" tileWidth tileHeight imgBaseDir

            else
                lever "off" tileWidth tileHeight imgBaseDir

        Tile.Water waterinfo ->
            water waterinfo tileWidth tileHeight imgBaseDir

        Tile.Grass grassinfo ->
            grass grassinfo tileWidth tileHeight imgBaseDir

        Tile.ConverterTile it ct ->
            tile currentFloorId tileWidth tileHeight imgBaseDir it

        _ ->
            notileyet tileWidth tileHeight


grass : Tile.GrassInfo -> Int -> Int -> String -> Collage Msg
grass grassinfo tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if grassinfo.description == "grass_with_dirt" then
                imgBaseDir ++ "/grass/grass_and_dirt.png"

            else
                imgBaseDir ++ "/grass/grass.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


tree : Tile.TreeInfo -> Int -> Int -> String -> Collage Msg
tree treeinfo tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if treeinfo.treeType == "pinetree" then
                imgBaseDir ++ "/trees/treetopPineTree_bg.png"

            else
                imgBaseDir ++ "/trees/treetopRoundTree_bg.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


water : Tile.WaterInfo -> Int -> Int -> String -> Collage Msg
water waterinfo tileWidth tileHeight imgBaseDir =
    let
        fileStr =
            if waterinfo.description == "water_wall_up" then
                imgBaseDir ++ "/water/water_wall_up.png"

            else if waterinfo.description == "water_wall_left" then
                imgBaseDir ++ "/water/water_wall_left.png"

            else
                imgBaseDir ++ "/water/just_water.png"
    in
    Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr


tileOverlay : Int -> Int -> String -> Tile -> Collage Msg
tileOverlay tileWidth tileHeight imgBaseDir t =
    case t of
        Tile.Floor floorinfo ->
            case floorinfo.item of
                Just Ash ->
                    floorOverlay "ash" tileWidth tileHeight imgBaseDir

                Just (Key keyinfo) ->
                    floorOverlay ("key_" ++ keyinfo.keyColor) tileWidth tileHeight imgBaseDir

                Just Box ->
                    Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/box.png")

                Just (Paper paperinfo) ->
                    if paperinfo.id == 1 then
                        Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/paper_part1.png")

                    else if paperinfo.id == 2 then
                        Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/paper_part2.png")

                    else
                        Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/paper_part3.png")

                Just (Food fdescription) ->
                    if String.toLower fdescription == "bread" then
                        Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/health_item.png")

                    else
                        Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/box.png")

                _ ->
                    case floorinfo.floorDrawing of
                        Just (Tile.LandingTargetDrawing nr) ->
                            floorOverlay "landingTarget" tileWidth tileHeight imgBaseDir

                        _ ->
                            floorOverlay "" tileWidth tileHeight imgBaseDir

        Tile.Wall wallinfo ->
            wallOverlay wallinfo tileWidth tileHeight imgBaseDir

        Tile.Door doorinfo ->
            doorOverlay doorinfo

        Tile.ConverterTile it ct ->
            tileOverlay tileWidth tileHeight imgBaseDir it

        Tile.NoTileYet ->
            notileyetOverlay

        _ ->
            notileyetOverlay


fogT : Int -> Int -> Tile.Visibility -> Collage Msg
fogT tileWidth tileHeight visibility =
    case visibility of
        Visible ->
            noForm

        Explored ->
            halfFog tileWidth tileHeight

        Unexplored ->
            fog tileWidth tileHeight


player : Int -> Int -> Collage Msg
player tileWidth tileHeight =
    Collage.circle (toFloat tileWidth / 2) |> filled (uniform red)


fightingCharacter : Int -> Int -> Collage Msg
fightingCharacter tileWidth tileHeight =
    Collage.circle (toFloat tileWidth / 2) |> filled (uniform green)


tCollageFromStr : String -> Collage Msg
tCollageFromStr elemStr =
    elemStr
        |> Text.fromString
        |> Text.color black
        |> Collage.rendered


playerImg : Player -> Tile.Visibility -> Int -> Int -> String -> Collage Msg
playerImg player_ visibility tileWidth tileHeight imgBaseDir =
    case visibility of
        Tile.Visible ->
            let
                fileStr =
                    case player_.direction of
                        Beings.Left ->
                            imgBaseDir ++ "/pc/left.png"

                        Beings.Right ->
                            imgBaseDir ++ "/pc/right.png"

                        Beings.Up ->
                            imgBaseDir ++ "/pc/up.png"

                        Beings.Down ->
                            imgBaseDir ++ "/pc/down.png"
            in
            Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr

        _ ->
            noForm


fightingCharacterView : Beings.FightingCharacter -> Bool -> Tile.Visibility -> Int -> Int -> String -> Collage Msg
fightingCharacterView fightChar showBlood visibility tileWidth tileHeight imgBaseDir =
    case visibility of
        Tile.Visible ->
            let
                fileStr =
                    if fightChar.indexOfLight >= fightChar.indexOfLightMax then
                        imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ "_enlightened.png"

                    else if fightChar.health > 0 then
                        imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ ".png"

                    else if fightChar.health <= 0 && showBlood then
                        imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ "_dead_blood.png"

                    else
                        imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ "_dead.png"
            in
            Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr

        _ ->
            noForm


otherCharacterView : Beings.OtherCharacter -> Bool -> Tile.Visibility -> Int -> Int -> String -> Collage Msg
otherCharacterView character showBlood visibility tileWidth tileHeight imgBaseDir =
    case visibility of
        Tile.Visible ->
            let
                fileStr =
                    imgBaseDir ++ "/pc/right.png"

                {-
                     if character.indexOfLight >= character.indexOfLightMax then
                         imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ "_enlightened.png"

                     else if character.health > 0 then
                         imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ ".png"

                     else if character.health <= 0 && showBlood then
                         imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ "_dead_blood.png"

                     else
                         imgBaseDir ++ "/characters/" ++ String.toLower fightChar.species ++ "_dead.png"
                   -
                -}
            in
            Collage.image ( toFloat tileWidth, toFloat tileHeight ) fileStr

        _ ->
            noForm


guy : { r | textAvatar : String } -> Tile.Visibility -> Int -> Int -> Collage Msg
guy r visibility tileWidth tileHeight =
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
                    min (divInts tileWidth xSize) (divInts tileHeight ySize)
            in
            scale factor form

        _ ->
            noForm


mainScreen : Model -> Collage Msg
mainScreen model =
    let
        ( subgrid, txtmsg ) =
            model.level
                |> Grid.getSubGrid model.viewport_topleft_x (model.viewport_topleft_x + model.window_width - 1) model.viewport_topleft_y (model.viewport_topleft_y + model.window_height - 1)

        ( wwidth, wheight ) =
            ( subgrid.size.width, subgrid.size.height )

        ( w, h ) =
            ( wwidth * model.tileWidth, wheight * model.tileHeight )

        xOffset : Int -> Float
        xOffset n =
            (toFloat n - toFloat model.viewport_topleft_x - toFloat wwidth / 2) * toFloat model.tileWidth

        yOffset : Int -> Float
        yOffset n =
            (toFloat n - toFloat model.viewport_topleft_y - toFloat wheight / 2) * toFloat model.tileHeight

        xOffset_for_subgrid : Int -> Float
        xOffset_for_subgrid n =
            (toFloat n - toFloat wwidth / 2) * toFloat model.tileWidth

        yOffset_for_subgrid : Int -> Float
        yOffset_for_subgrid n =
            (toFloat n - toFloat wheight / 2) * toFloat model.tileHeight

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
            playerImg model.player Tile.Visible model.tileWidth model.tileHeight (getImgBaseDir model)
                |> shift (location model.player)

        fightingCharacter_ =
            let
                relevantFightingCharactersDict =
                    Dict.filter (\fcharId fightChar -> (fightChar.floorId == model.currentFloorId) && (fightChar.location.x >= model.viewport_topleft_x && fightChar.location.x - model.viewport_topleft_x < model.window_width) && (fightChar.location.y >= model.viewport_topleft_y && fightChar.location.y - model.viewport_topleft_y < model.window_height)) model.fightingCharacters

                mkfightingCharacter fcharId anfightingCharacter =
                    fightingCharacterView anfightingCharacter model.showBlood (GameModel.getGridTileVisibility anfightingCharacter.location model.level) model.tileWidth model.tileHeight (getImgBaseDir model)
                        |> shift (location anfightingCharacter)
            in
            group <| (Dict.map mkfightingCharacter relevantFightingCharactersDict |> Dict.values)

        otherCharacters_ =
            let
                relevantOtherCharsDict =
                    Dict.filter (\charId char -> (char.floorId == model.currentFloorId) && (char.location.x >= model.viewport_topleft_x && char.location.x - model.viewport_topleft_x < model.window_width) && (char.location.y >= model.viewport_topleft_y && char.location.y - model.viewport_topleft_y < model.window_height)) model.otherCharacters

                mkOtherChar ch_id achar =
                    otherCharacterView achar model.showBlood (GameModel.getGridTileVisibility achar.location model.level) model.tileWidth model.tileHeight (getImgBaseDir model)
                        |> shift (location achar)
            in
            group <| (Dict.map mkOtherChar relevantOtherCharsDict |> Dict.values)

        bg : Collage Msg
        bg =
            Collage.group
                [ mkLayer (Grid.toList subgrid) (row (tileOverlay model.tileWidth model.tileHeight (getImgBaseDir model)))
                , mkLayer (Grid.toList subgrid) (row (tile model.currentFloorId model.tileWidth model.tileHeight (getImgBaseDir model)))
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
            mkLayer (Grid.toList visibilitySubGrid) (row (fogT model.tileWidth model.tileHeight))
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
            , "viewport_topleft_x: " ++ String.fromInt model.viewport_topleft_x |> Text.fromString |> theColor |> Collage.rendered
            , "viewport_topleft_y: " ++ String.fromInt model.viewport_topleft_y |> Text.fromString |> theColor |> Collage.rendered
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


viewItem : Item.Item -> Int -> Int -> String -> Collage Msg
viewItem item tileWidth tileHeight imgBaseDir =
    case item of
        Key { keyColor } ->
            Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/key_" ++ String.toLower keyColor ++ ".png")

        Box ->
            Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/box.png")

        Paper paperinfo ->
            if paperinfo.id == 1 then
                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/paper_part1.png")

            else if paperinfo.id == 2 then
                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/paper_part2.png")

            else
                Collage.image ( toFloat tileWidth, toFloat tileHeight ) (imgBaseDir ++ "/items/paper_part3.png")

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
            List.map (\it -> viewItem it model.tileWidth model.tileHeight (getImgBaseDir model)) (Dict.values model.player.inventory)

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
                (List.concatMap (\it -> [ Html.a [] [ Html.img [ Attr.size 640, Attr.src (getImgBaseDir model ++ Item.itemToImgSrc it) ] [] ], Html.br [] [] ]) (Dict.values model.player.inventory))
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
                |> Grid.getSubGrid model.viewport_topleft_x (model.viewport_topleft_x + model.window_width - 1) model.viewport_topleft_y (model.viewport_topleft_y + model.window_height - 1)
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
    let
        fileStr fightChar =
            if fightChar.indexOfLight >= fightChar.indexOfLightMax then
                getImgBaseDir model ++ "/characters/" ++ String.toLower fightChar.species ++ "_enlightened.png"

            else if fightChar.health > 0 then
                getImgBaseDir model ++ "/characters/" ++ String.toLower fightChar.species ++ ".png"

            else if fightChar.health <= 0 && model.showBlood then
                getImgBaseDir model ++ "/characters/" ++ String.toLower fightChar.species ++ "_dead_blood.png"

            else
                getImgBaseDir model ++ "/characters/" ++ String.toLower fightChar.species ++ "_dead.png"

        fcharLine fightChar =
            [ Html.span [] [ Html.img [ Attr.width 50, Attr.height 50, Attr.src (fileStr fightChar) ] [] ]

            -- "name : " ++ fightChar.name ++
            , Html.span [] [ Html.text ("    ,    health : " ++ String.fromInt fightChar.health ++ "  ,     IndexOfLight : " ++ String.fromInt fightChar.indexOfLight), Html.br [] [], Html.br [] [], Html.br [] [], Html.br [] [] ]
            ]
    in
    Html.div [ Attr.align "center" ]
        [ Html.h3 [] [ Html.text "Opponent Report :" ]
        , Html.br [] []
        , Html.div
            []
            (Dict.values model.fightingCharacters |> List.concatMap (\fightChar_ -> fcharLine fightChar_))
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
        , Html.text "Find your way through the Caverns, Basement , The Ground Floor , First Floor and the Attic , pick up the three pieces of paper with the codes to unlock the striped door and move towards enlightenment !!!"
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
            viewStartMenuChoices model (getImgBaseDir model)


getImgBaseDir : Model -> String
getImgBaseDir model =
    model.imgBaseDir
        |> Maybe.withDefault "./img"


viewGameOfThorns : Model -> Html GameUpdate.Msg
viewGameOfThorns model =
    Html.div [] [ Html.map GameUpdate.ThornsMsg (ThornsView.view model.gameOfThornsModel) ]


viewStartMenuChoices : Model -> String -> Html GameUpdate.Msg
viewStartMenuChoices model imgBaseDir =
    Html.div [ Attr.align "center" ]
        [ Html.div [ Attr.align "center" ]
            [ Html.h3 []
                [ Html.a
                    [ Html.Events.onClick (GameUpdate.StartGameNr 2) ]
                    [ Html.text "Start - Castle of Elm Tribulations" ]
                , Html.br [] []
                , Html.a
                    [ Html.Events.onClick (GameUpdate.StartGameNr 2) ]
                    [ Html.img [ Attr.src (imgBaseDir ++ "/game/casteleOfElmTribulations_.png") ] [] ]
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
