module GameView exposing
    ( acid
    , acidOverlay
    , display
    , door
    , doorOverlay
    , enemy
    , floor
    , floorOverlay
    , floor_
    , fog
    , fogT
    , gridToHtmlList
    , guy
    , halfFog
    , lever
    , mainScreen
    , noForm
    , notileyet
    , notileyetOverlay
    , player
    , sidebar
    , tile
    , tileOverlay
    , view
    , viewDebugEnemies
    , viewDebugGrid
    , viewDebugPlayer
    , wall
    , wallOverlay
    , xScale
    , yScale
    )

--import Element exposing (..)

import Beings exposing (Enemy, EnemyId, OPPONENT_INTERACTION_OPTIONS(..), Player)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text as Text
import Color exposing (..)
import Debug
import Dict exposing (Dict)
import GameModel
import GameUpdate
import Grid
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Item exposing (Item(..), KeyInfo)
import Thorns.Types exposing (Msg(..))
import Thorns.View as ThornsView


xScale : Int
xScale =
    --15 * 3
    64


yScale : Int
yScale =
    --20 * 3
    64



--Form


noForm : Collage msg
noForm =
    --toForm empty
    Collage.circle 0 |> filled (uniform red)


floor_ :
    GameModel.FloorInfo
    -> Collage msg --Form
floor_ floorinfo =
    --rect (toFloat xScale) (toFloat yScale) |> filled black
    let
        fileStr =
            if floorinfo.color == "orange" then
                -- "orange" then
                --"img/floor/floor_01_orange.png"
                "img/floor/floor_01.png"

            else
                "img/floor/floor_01.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


floor : GameModel.FloorInfo -> Collage msg
floor floorinfo =
    {- }
       let
           theGradient =
               linear ( 1.0, 30.0 ) ( 60.0, 30.0 ) [ ( 0, rgb 100 100 100 ), ( 0.3, rgb 150 100 100 ), ( 0.5, rgb 200 100 100 ), ( 0.7, rgb 220 100 100 ), ( 1, rgb 250 100 100 ) ]
       in
    -}
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform blue)



--|> gradient theGradient


floorOverlay : String -> Collage msg
floorOverlay elemStr =
    --guy { avatar = "." |> Text.fromString |> Text.monospace |> Text.color white |> centered } GameModel.Visible
    if elemStr == "ash" then
        Collage.image ( toFloat (xScale - 30), toFloat (yScale - 30) ) "img/floor/floor_ash.png"

    else if String.startsWith "key" elemStr then
        Collage.image ( toFloat (xScale - 30), toFloat (yScale - 30) ) ("img/items/" ++ elemStr ++ ".png")

    else if elemStr == "landingTarget" then
        Collage.image ( toFloat (xScale - 30), toFloat (yScale - 30) ) "img/floor/floor_landing_target.png"

    else
        noForm


stairs : String -> Collage msg
stairs upOrDownStr =
    let
        fileStr =
            if String.toLower upOrDownStr == "up" then
                "img/floor/floor_stairs_up.png"

            else
                "img/floor/floor_stairs_down.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


hole : Collage msg
hole =
    let
        fileStr =
            "img/floor/floor_hole.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


wall : String -> Collage msg
wall orientationStr =
    --rect (toFloat xScale) (toFloat yScale) |> filled grey
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

        --"img/walls/empty-flat-empty-flat.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


wallOverlay : GameModel.WallInfo -> Collage msg
wallOverlay wallinfo =
    --guy { avatar = "#" |> Text.fromString |> Text.monospace |> Text.color black |> centered } GameModel.Visible
    case wallinfo.mbTeleporterObject of
        Just tinfo ->
            case tinfo.teleporterType of
                GameModel.Barrel ->
                    Collage.image ( toFloat xScale, toFloat yScale ) "img/walls/wall_overlay_teleporter_barrel_up.png"

                GameModel.BookCase ->
                    Collage.image ( toFloat xScale, toFloat yScale ) "img/walls/wall_overlay_teleporter_bookcase_up.png"

                GameModel.Clock ->
                    Collage.image ( toFloat xScale, toFloat yScale ) "img/walls/wall_overlay_teleporter_clock_up.png"

        _ ->
            noForm


door : GameModel.DoorInfo -> Collage msg
door doorinfo =
    if doorinfo.isOpen then
        rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform white)

    else
        rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform (doorinfo.color |> Maybe.withDefault "white" |> stringToColor))


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


doorOverlay : Collage msg
doorOverlay =
    noForm


lever : String -> Collage msg
lever onOffStr =
    let
        fileStr =
            if onOffStr == "on" || onOffStr == "On" || onOffStr == "ON" then
                "img/levers/lever_color_on.png"

            else
                "img/levers/lever_color_off.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


acid : Collage msg
acid =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform darkGreen)


acidOverlay : Collage msg
acidOverlay =
    noForm


notileyet : Collage msg
notileyet =
    --rect (toFloat xScale) (toFloat yScale) |> filled blue
    --gradient : Gradient -> Shape -> Form
    {- }
       let
           theGradient =
               linear ( 1.0, 1.0 ) ( 50.0, 50.0 ) [ ( 0, rgb 100 100 100 ), ( 0.3, rgb 150 100 100 ), ( 0.5, rgb 200 100 100 ), ( 0.7, rgb 220 100 100 ), ( 1, rgb 250 100 100 ) ]
       in
    -}
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform black)



--|> gradient theGradient


notileyetOverlay : Collage msg
notileyetOverlay =
    noForm


fog : Collage msg
fog =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform (rgba 0 0 0 1))


halfFog : Collage msg
halfFog =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform (rgba 0 0 0 0.6))


tile : Int -> GameModel.Tile -> Collage msg
tile currentFloorId t =
    case t of
        GameModel.Floor floorinfo ->
            --floor floorinfo
            floor_ floorinfo

        GameModel.Stairs sinfo ->
            if sinfo.toFloorId > currentFloorId then
                stairs "up"

            else
                stairs "down"

        GameModel.Hole hinfo ->
            hole

        GameModel.Wall wallinfo ->
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

        GameModel.Door doorinfo ->
            door doorinfo

        GameModel.NoTileYet ->
            notileyet

        GameModel.Lever leverinfo ->
            if leverinfo.isUp then
                lever "on"

            else
                lever "off"

        GameModel.Water waterinfo ->
            water waterinfo

        GameModel.Grass grassinfo ->
            grass grassinfo

        _ ->
            notileyet


grass : GameModel.GrassInfo -> Collage msg
grass grassinfo =
    let
        fileStr =
            if grassinfo.description == "grass_with_dirt" then
                "img/grass/grass_and_dirt.png"

            else
                "img/grass/grass.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


water : GameModel.WaterInfo -> Collage msg
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


tileOverlay : GameModel.Tile -> Collage msg
tileOverlay t =
    case t of
        GameModel.Floor floorinfo ->
            case floorinfo.item of
                Just Ash ->
                    floorOverlay "ash"

                Just (Key keyinfo) ->
                    --floorOverlay ( "key_" ++ floorinfo.item.color )
                    floorOverlay ("key_" ++ keyinfo.keyColor)

                _ ->
                    case floorinfo.floorDrawing of
                        Just (GameModel.LandingTargetDrawing nr) ->
                            floorOverlay "landingTarget"

                        _ ->
                            floorOverlay ""

        GameModel.Wall wallinfo ->
            wallOverlay wallinfo

        GameModel.Door doorinfo ->
            doorOverlay

        GameModel.NoTileYet ->
            notileyetOverlay

        _ ->
            notileyetOverlay


fogT : GameModel.Visibility -> Collage msg
fogT visibility =
    case visibility of
        GameModel.Visible ->
            noForm

        GameModel.Explored ->
            halfFog

        GameModel.Unexplored ->
            fog


player : Collage msg
player =
    Collage.circle (toFloat xScale / 2) |> filled (uniform red)


enemy : Collage msg
enemy =
    Collage.circle (toFloat xScale / 2) |> filled (uniform green)


tCollageFromStr : String -> Collage msg
tCollageFromStr elemStr =
    elemStr
        |> Text.fromString
        --|> Text.monospace
        |> Text.color black
        |> Collage.rendered



--|> centered


guy : { r | textAvatar : String } -> GameModel.Visibility -> Collage msg
guy r visibility =
    case visibility of
        GameModel.Visible ->
            let
                form =
                    r.textAvatar |> tCollageFromStr

                --|> toForm
                ( xSize, ySize ) =
                    --sizeOf r.textAvatar
                    ( 16, 16 )

                --x /// y = toFloat x / toFloat y -- This is a convenience function to divide two ints
                divInts x y =
                    toFloat x / toFloat y

                --factor = min (xScale /// xSize) (yScale /// ySize)
                factor =
                    min (divInts xScale xSize) (divInts yScale ySize)
            in
            scale factor form

        _ ->
            noForm



{- }
   text : String -> Collage msg
   text =
       Text.fromString >> Text.monospace >> Text.color white >> centered
-}


mainScreen : GameModel.Model -> Collage msg
mainScreen model =
    let
        ( subgrid, txtmsg ) =
            model.level
                |> Grid.getSubGrid model.x_display_anchor (model.x_display_anchor + model.window_width - 1) model.y_display_anchor (model.y_display_anchor + model.window_height - 1)

        --subgridList =
        --    subgrid
        --        |> Grid.toList
        ( wwidth, wheight ) =
            --( model.window_width, model.window_height )
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

        mkLayer : List (List a) -> (( Int, List a ) -> List (Collage msg)) -> Collage msg
        mkLayer agrid mapRow =
            let
                rows =
                    List.map2 (\v1 v2 -> ( v1, v2 )) (List.reverse (List.range 0 (wheight - 1))) agrid

                forms =
                    List.concatMap mapRow rows
            in
            --collage (w + xScale) (h + yScale) forms
            Collage.group forms

        row : (a -> Collage msg) -> ( Int, List a ) -> List (Collage msg)
        row mkTile ( n, tiles ) =
            let
                tiles_ =
                    List.map2 (\v1 v2 -> ( v1, v2 )) (List.range 0 (wwidth - 1)) tiles

                makeTile ( n_, t ) =
                    shift ( xOffset_for_subgrid n_, yOffset_for_subgrid n ) <| mkTile t
            in
            List.map makeTile tiles_

        player_ =
            guy model.player GameModel.Visible |> shift (location model.player)

        enemy_ =
            let
                relevantEnemiesDict =
                    Dict.filter (\enId enem -> (enem.location.x >= model.x_display_anchor && enem.location.x - model.x_display_anchor < model.window_width) && (enem.location.y >= model.y_display_anchor && enem.location.y - model.y_display_anchor < model.window_height)) model.enemies

                mkEnemy enid anenemy =
                    --guy enemy (GameModel.getGridTileVisibility (GameModel.tupleFloatsToLocation (location enemy)) subgrid)
                    guy anenemy (GameModel.getGridTileVisibility anenemy.location model.level)
                        |> shift (location anenemy)
            in
            group <| (Dict.map mkEnemy relevantEnemiesDict |> Dict.values)

        --grid =
        --Grid.toList model.level
        bg : Collage msg
        bg =
            --background model.level model
            --background subgrid model
            --layers [ mkLayer (Grid.toList subgrid) (row tile), mkLayer (Grid.toList subgrid) (row tileOverlay) ]
            Collage.group
                [ mkLayer (Grid.toList subgrid) (row tileOverlay)
                , mkLayer (Grid.toList subgrid) (row (tile model.currentFloorId))
                ]
                |> name "background"

        pos =
            locate "background" topLeft bg
                |> Maybe.withDefault ( -100000, -100000 )

        --|> Debug.log "background topLeft is : "
        pg =
            --collage (w + xScale) (h + yScale) [ player_, enemy_ ]
            Collage.group
                [ player_ |> shift ( 0, 0 )

                --  , enemy_ |> shift ( 0, 0 )
                ]

        eg =
            Collage.group
                [ enemy_ ]

        emptyg =
            Collage.group
                []

        visibilitySubGrid =
            Grid.map (\t -> GameModel.getTileVisibility t) subgrid

        fogger =
            mkLayer (Grid.toList visibilitySubGrid) (row fogT)
    in
    --flow down
    Collage.group
        ([ --layers [ bg, pg, fogger ]
           fogger
         , pg |> shift ( 0, 0 )
         , eg
         , bg

         --, fogger
         --layers [ bg, pg ]
         --flow down <| List.map text (List.take 3 model.log)
         ]
         --  ++ List.map (Text.fromString >> Collage.rendered) (List.take 3 model.log)
        )
        |> name "mainScreen"


inViewRange : Enemy -> Bool
inViewRange enemy_ =
    False



{-
   background : Grid.Grid GameModel.Tile -> GameModel.Model -> Element
   background subgrid model =
       let
           grid =
               subgrid
                   |> Grid.toList

           ( wwidth, wheight ) =
               --( model.window_width, model.window_height )
               ( subgrid.size.width, subgrid.size.height )

           ( w, h ) =
               ( wwidth * xScale, wheight * yScale )

           xOffset : Int -> Float
           xOffset n =
               (toFloat n - toFloat wwidth / 2) * toFloat xScale

           yOffset : Int -> Float
           yOffset n =
               (toFloat n - toFloat wheight / 2) * toFloat yScale

           mkLayer : List (List a) -> (( Int, List a ) -> List Form) -> Element
           mkLayer grid mapRow =
               let
                   rows =
                       List.map2 (,) (List.reverse (List.range 0 (wheight - 1))) grid

                   forms =
                       List.concatMap mapRow rows
               in
               collage (w + xScale) (h + yScale) forms

           row : (a -> Form) -> ( Int, List a ) -> List Form
           row mkTile ( n, tiles ) =
               let
                   tiles_ =
                       List.map2 (,) (List.range 0 (wwidth - 1)) tiles

                   makeTile ( n_, t ) =
                       move ( xOffset n_, yOffset n ) <| mkTile t
               in
               List.map makeTile tiles_
       in
       layers [ mkLayer grid (row tile), mkLayer grid (row tileOverlay) ]

-}


sidebar : GameModel.Model -> ( Float, Float ) -> Collage msg
sidebar model pos =
    let
        x =
            5

        theColor =
            Text.color white

        theColor2 =
            Text.color red

        bar =
            --flow down
            [ model.player.textAvatar ++ " : " ++ model.player.name |> Text.fromString |> theColor |> Collage.rendered
            , "Health: " ++ String.fromInt model.player.health |> Text.fromString |> theColor |> Collage.rendered
            , "Energy: " ++ String.fromInt model.player.energy |> Text.fromString |> theColor |> Collage.rendered
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
                |> List.indexedMap (\i elem -> shift ( -400, 200 - toFloat i * 25 ) elem)
                |> Collage.group
    in
    --container (widthOf bar + 20) (heightOf bar) midTop bar
    bar


display :
    GameModel.Model
    -> Collage msg --Element
display model =
    let
        pos =
            locate "mainScreen" topLeft (mainScreen model)
                |> Maybe.withDefault ( -100000, -100000 )

        --|> Debug.log "mainScreen topLeft is : "
    in
    --flow right [ sidebar model, mainScreen model ] |> color black
    Collage.group
        [ sidebar model pos

        --|> shift ( -model.x_display_anchor * xScale |> toFloat >> (\x -> x * 0.5), model.y_display_anchor * yScale |> toFloat )
        , mainScreen model

        --|> shift pos --|> shift ( -model.x_display_anchor * xScale |> toFloat, -model.y_display_anchor * yScale |> toFloat )
        ]



--|> color black


gridToHtmlList : Grid.Grid a -> List (Html msg)
gridToHtmlList grid =
    let
        lofls =
            Grid.toList grid

        funcListToString l =
            --List.foldl (\x y -> y ++ " , " ++ toString x) "" l
            List.foldl (\x y -> y ++ " , " ++ "Todo ... convert grid element to string ") "" l

        lofstrs =
            List.map (\x -> funcListToString x) lofls
    in
    List.map (\astr -> Html.h1 [] [ Html.text astr ]) lofstrs


viewDebugGrid : Grid.Grid a -> GameModel.Model -> List (Html msg)
viewDebugGrid grid model =
    let
        --_ =
        --    Debug.log "viewDebugGrid has been called "
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


viewDebugPlayer : GameModel.Model -> Html msg
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


viewDebugEnemies : GameModel.Model -> List (Html msg)
viewDebugEnemies model =
    let
        enemyToHtmlFunc enemy_ enemyId =
            Html.h2 []
                [ Html.text
                    ("enemy "
                        ++ String.fromInt enemyId
                        ++ " is in position "
                        ++ String.fromInt enemy_.location.x
                        ++ " , "
                        ++ String.fromInt enemy_.location.y
                        ++ " , has health = "
                        ++ String.fromInt enemy_.health
                    )
                ]
    in
    Dict.map (\enid enemy_ -> enemyToHtmlFunc enemy_ enid) model.enemies
        |> Dict.values


view : GameModel.Model -> Html GameUpdate.Msg
view model =
    case model.started of
        True ->
            Html.div []
                [ if not model.gameOfThornsModeisOn then
                    Html.div []
                        ([ display model
                            |> svg
                         ]
                         --++ [ viewDebugPlayer model ]
                         --  ++ viewDebugEnemies model
                         --  ++ viewDebugGrid model.level model
                        )

                  else
                    viewGameOfThorns model
                ]

        False ->
            viewStartMenuChoices model


viewGameOfThorns : GameModel.Model -> Html GameUpdate.Msg
viewGameOfThorns model =
    Html.div [] [ Html.map GameUpdate.ThornsMsg (ThornsView.view model.gameOfThornsModel) ]


viewStartMenuChoices : GameModel.Model -> Html GameUpdate.Msg
viewStartMenuChoices model =
    Html.div []
        [ Html.div []
            [ Html.h3 []
                [ Html.a [ Html.Events.onClick (GameUpdate.StartGameNr 1) ] [ Html.text "Start Game 1 - Random Dungeon " ]
                ]
            ]
        , Html.br [] []
        , Html.div []
            [ Html.h3 []
                [ Html.a [ Html.Events.onClick (GameUpdate.StartGameNr 2) ] [ Html.text "Start Game 2 - Atic Atac Style  Dungeon " ]
                ]
            ]
        ]
