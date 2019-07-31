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

    else
        noForm


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

            else
                --"img/walls/wall.png"
                "img/walls/empty-flat-empty-flat.png"
    in
    Collage.image ( toFloat xScale, toFloat yScale ) fileStr


wallOverlay : Collage msg
wallOverlay =
    --guy { avatar = "#" |> Text.fromString |> Text.monospace |> Text.color black |> centered } GameModel.Visible
    noForm


door : Collage msg
door =
    rectangle (toFloat xScale) (toFloat yScale) |> filled (uniform purple)


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


tile : GameModel.Tile -> Collage msg
tile t =
    case t of
        GameModel.Floor floorinfo ->
            --floor floorinfo
            floor_ floorinfo

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

            else
                wall "horizontal"

        GameModel.Door doorinfo ->
            door

        GameModel.NoTileYet ->
            notileyet

        GameModel.Lever leverinfo ->
            if leverinfo.isUp then
                lever "on"

            else
                lever "off"

        _ ->
            notileyet


tileOverlay : GameModel.Tile -> Collage msg
tileOverlay t =
    case t of
        GameModel.Floor floorinfo ->
            case floorinfo.item of
                Just GameModel.Ash ->
                    floorOverlay "ash"

                _ ->
                    floorOverlay ""

        GameModel.Wall wallinfo ->
            wallOverlay

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


mainScreen : GameModel.State -> Collage msg
mainScreen state =
    let
        ( subgrid, txtmsg ) =
            state.level
                |> Grid.getSubGrid state.x_display_anchor (state.x_display_anchor + state.window_width - 1) state.y_display_anchor (state.y_display_anchor + state.window_height - 1)

        --subgridList =
        --    subgrid
        --        |> Grid.toList
        ( wwidth, wheight ) =
            --( state.window_width, state.window_height )
            ( subgrid.size.width, subgrid.size.height )

        ( w, h ) =
            ( wwidth * xScale, wheight * yScale )

        xOffset : Int -> Float
        xOffset n =
            (toFloat n - toFloat state.x_display_anchor - toFloat wwidth / 2) * toFloat xScale

        yOffset : Int -> Float
        yOffset n =
            (toFloat n - toFloat state.y_display_anchor - toFloat wheight / 2) * toFloat yScale

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
            guy state.player GameModel.Visible |> shift (location state.player)

        enemy_ =
            let
                relevantEnemiesDict =
                    Dict.filter (\enId enem -> (enem.location.x >= state.x_display_anchor && enem.location.x - state.x_display_anchor < state.window_width) && (enem.location.y >= state.y_display_anchor && enem.location.y - state.y_display_anchor < state.window_height)) state.enemies

                mkEnemy enid anenemy =
                    --guy enemy (GameModel.getGridTileVisibility (GameModel.tupleFloatsToLocation (location enemy)) subgrid)
                    guy anenemy (GameModel.getGridTileVisibility anenemy.location state.level)
                        |> shift (location anenemy)
            in
            group <| (Dict.map mkEnemy relevantEnemiesDict |> Dict.values)

        --grid =
        --Grid.toList state.level
        bg : Collage msg
        bg =
            --background state.level state
            --background subgrid state
            --layers [ mkLayer (Grid.toList subgrid) (row tile), mkLayer (Grid.toList subgrid) (row tileOverlay) ]
            Collage.group
                [ mkLayer (Grid.toList subgrid) (row tileOverlay)
                , mkLayer (Grid.toList subgrid) (row tile)
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
         --flow down <| List.map text (List.take 3 state.log)
         ]
         --  ++ List.map (Text.fromString >> Collage.rendered) (List.take 3 state.log)
        )
        |> name "mainScreen"


inViewRange : GameModel.Enemy -> Bool
inViewRange enemy_ =
    False



{-
   background : Grid.Grid GameModel.Tile -> GameModel.State -> Element
   background subgrid state =
       let
           grid =
               subgrid
                   |> Grid.toList

           ( wwidth, wheight ) =
               --( state.window_width, state.window_height )
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


sidebar : GameModel.State -> ( Float, Float ) -> Collage msg
sidebar state pos =
    let
        x =
            5

        theColor =
            Text.color white

        bar =
            --flow down
            [ state.player.textAvatar ++ " : " ++ state.player.name |> Text.fromString |> theColor |> Collage.rendered
            , "Health: " ++ String.fromInt state.player.health |> Text.fromString |> theColor |> Collage.rendered
            , "Energy: " ++ String.fromInt state.player.energy |> Text.fromString |> theColor |> Collage.rendered
            , "Hunger: " ++ String.fromInt state.player.hunger |> Text.fromString |> theColor |> Collage.rendered
            , "Stealth: " ++ String.fromInt state.player.stealth ++ "%" |> Text.fromString |> theColor |> Collage.rendered
            , "Armor: " ++ String.fromInt state.player.armor |> Text.fromString |> theColor |> Collage.rendered
            , "Protection: " ++ String.fromInt state.player.protection ++ "%" |> Text.fromString |> theColor |> Collage.rendered
            , "Coordination: " ++ String.fromInt state.player.coordination ++ "%" |> Text.fromString |> theColor |> Collage.rendered
            , "Power: " ++ String.fromInt state.player.power |> Text.fromString |> theColor |> Collage.rendered
            , "Initiative: " ++ String.fromInt state.player.initiative |> Text.fromString |> theColor |> Collage.rendered
            , "x_display_anchor: " ++ String.fromInt state.x_display_anchor |> Text.fromString |> theColor |> Collage.rendered
            , "y_display_anchor: " ++ String.fromInt state.y_display_anchor |> Text.fromString |> theColor |> Collage.rendered
            , "current_player_x : " ++ String.fromInt state.player.location.x |> Text.fromString |> theColor |> Collage.rendered
            , "current_player_y : " ++ String.fromInt state.player.location.y |> Text.fromString |> theColor |> Collage.rendered
            , "wall percentage : " ++ String.fromFloat (state.wallPercentage |> Maybe.withDefault 0) |> Text.fromString |> theColor |> Collage.rendered
            ]
                |> List.indexedMap (\i elem -> shift ( -400, 200 - toFloat i * 25 ) elem)
                |> Collage.group
    in
    --container (widthOf bar + 20) (heightOf bar) midTop bar
    bar


display :
    GameModel.State
    -> Collage msg --Element
display state =
    let
        pos =
            locate "mainScreen" topLeft (mainScreen state)
                |> Maybe.withDefault ( -100000, -100000 )

        --|> Debug.log "mainScreen topLeft is : "
    in
    --flow right [ sidebar state, mainScreen state ] |> color black
    Collage.group
        [ sidebar state pos

        --|> shift ( -state.x_display_anchor * xScale |> toFloat >> (\x -> x * 0.5), state.y_display_anchor * yScale |> toFloat )
        , mainScreen state

        --|> shift pos --|> shift ( -state.x_display_anchor * xScale |> toFloat, -state.y_display_anchor * yScale |> toFloat )
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


viewDebugGrid : Grid.Grid a -> GameModel.State -> List (Html msg)
viewDebugGrid grid state =
    let
        _ =
            Debug.log "viewDebugGrid has been called "

        ( subgrid, txtmsg ) =
            grid
                |> Grid.getSubGrid state.x_display_anchor (state.x_display_anchor + state.window_width - 1) state.y_display_anchor (state.y_display_anchor + state.window_height - 1)
    in
    [ Html.div []
        ([ Html.h1 [] [ Html.text ("viewDebugGrid has been called with : " ++ txtmsg) ] ]
            ++ (subgrid
                    |> gridToHtmlList
               )
        )
    ]


viewDebugPlayer : GameModel.State -> Html msg
viewDebugPlayer state =
    Html.h2 []
        [ Html.text
            ("player is in position "
                ++ String.fromInt state.player.location.x
                ++ " , "
                ++ String.fromInt state.player.location.y
                ++ " , has health = "
                ++ String.fromInt state.player.health
                ++ " , and energy = "
                ++ String.fromInt state.player.energy
            )
        ]


viewDebugEnemies : GameModel.State -> List (Html msg)
viewDebugEnemies state =
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
    Dict.map (\enid enemy_ -> enemyToHtmlFunc enemy_ enid) state.enemies
        |> Dict.values


view : GameModel.State -> Html GameUpdate.Msg
view model =
    case model.started of
        True ->
            Html.div []
                ([ display model
                    --|> Element.toHtml
                    |> svg
                 ]
                 --++ [ viewDebugPlayer model ]
                 --  ++ viewDebugEnemies model
                 --  ++ viewDebugGrid model.level model
                )

        False ->
            viewStartMenuChoices model


viewStartMenuChoices : GameModel.State -> Html GameUpdate.Msg
viewStartMenuChoices model =
    Html.div []
        [ Html.div [] [ Html.a [ Html.Events.onClick (GameUpdate.StartGameNr 1) ] [ Html.text "Start Game 1 - Random Dungeon " ] ]
        , Html.br [] []
        , Html.div [] [ Html.a [ Html.Events.onClick (GameUpdate.StartGameNr 2) ] [ Html.text "Start Game 2 - Atic Atac Style  Dungeon " ] ]
        ]
