module GameView exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Debug
import Dict exposing (Dict)
import Element exposing (..)
import GameModel
import GameUpdate
import Grid
import Html exposing (Html)
import Text


xScale : Int
xScale =
    --15 * 3
    64


yScale : Int
yScale =
    --20 * 3
    64


noForm : Form
noForm =
    toForm empty


floor : Form
floor =
    --rect (toFloat xScale) (toFloat yScale) |> filled black
    Element.image xScale yScale "img/floor/floor_01.png"
        |> Collage.toForm


floorOverlay : Form
floorOverlay =
    --guy { avatar = "." |> Text.fromString |> Text.monospace |> Text.color white |> centered } GameModel.Visible
    noForm


wall : Form
wall =
    --rect (toFloat xScale) (toFloat yScale) |> filled grey
    Element.image xScale yScale "img/walls/wall.png"
        |> Collage.toForm


wallOverlay : Form
wallOverlay =
    --guy { avatar = "#" |> Text.fromString |> Text.monospace |> Text.color black |> centered } GameModel.Visible
    noForm


door : Form
door =
    rect (toFloat xScale) (toFloat yScale) |> filled purple


doorOverlay : Form
doorOverlay =
    noForm


acid : Form
acid =
    rect (toFloat xScale) (toFloat yScale) |> filled darkGreen


acidOverlay : Form
acidOverlay =
    noForm


notileyet : Form
notileyet =
    rect (toFloat xScale) (toFloat yScale) |> filled blue


notileyetOverlay : Form
notileyetOverlay =
    noForm


fog : Form
fog =
    rect (toFloat xScale) (toFloat yScale) |> filled (rgba 0 0 0 1)


halfFog : Form
halfFog =
    rect (toFloat xScale) (toFloat yScale) |> filled (rgba 0 0 0 0.7)


tile : GameModel.Tile -> Form
tile t =
    case t of
        GameModel.Floor floorinfo ->
            floor

        GameModel.Wall wallinfo ->
            wall

        GameModel.Door doorinfo ->
            door

        GameModel.NoTileYet ->
            notileyet

        _ ->
            notileyet


tileOverlay : GameModel.Tile -> Form
tileOverlay t =
    case t of
        GameModel.Floor floorinfo ->
            floorOverlay

        GameModel.Wall wallinfo ->
            wallOverlay

        GameModel.Door doorinfo ->
            doorOverlay

        GameModel.NoTileYet ->
            notileyetOverlay

        _ ->
            notileyetOverlay


fogT : GameModel.Visibility -> Form
fogT visibility =
    case visibility of
        GameModel.Visible ->
            noForm

        GameModel.Explored ->
            halfFog

        GameModel.Unexplored ->
            fog


player : Form
player =
    circle (toFloat xScale / 2) |> filled red


enemy : Form
enemy =
    circle (toFloat xScale / 2) |> filled green


guy : { r | avatar : Element } -> GameModel.Visibility -> Form
guy r visibility =
    case visibility of
        GameModel.Visible ->
            let
                form =
                    r.avatar |> toForm

                ( xSize, ySize ) =
                    sizeOf r.avatar

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


text : String -> Element
text =
    Text.fromString >> Text.monospace >> Text.color white >> centered


mainScreen : GameModel.State -> Element
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

        mkLayer : List (List a) -> (( Int, List a ) -> List Form) -> Element
        mkLayer agrid mapRow =
            let
                rows =
                    List.map2 (,) (List.reverse (List.range 0 (wheight - 1))) agrid

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
                    move ( xOffset_for_subgrid n_, yOffset_for_subgrid n ) <| mkTile t
            in
            List.map makeTile tiles_

        player_ =
            guy state.player GameModel.Visible |> move (location state.player)

        enemy_ =
            let
                mkEnemy enid enemy =
                    --guy enemy (GameModel.getGridTileVisibility (GameModel.tupleFloatsToLocation (location enemy)) subgrid)
                    guy enemy (GameModel.getGridTileVisibility enemy.location state.level)
                        |> move (location enemy)
            in
            group <| (Dict.map mkEnemy state.enemies |> Dict.values)

        --grid =
        --Grid.toList state.level
        bg : Element
        bg =
            --background state.level state
            --background subgrid state
            layers [ mkLayer (Grid.toList subgrid) (row tile), mkLayer (Grid.toList subgrid) (row tileOverlay) ]

        pg =
            collage (w + xScale) (h + yScale) [ player_, enemy_ ]

        visibilitySubGrid =
            Grid.map (\t -> GameModel.getTileVisibility t) subgrid

        fogger =
            mkLayer (Grid.toList visibilitySubGrid) (row fogT)
    in
    flow down
        [ layers [ bg, pg, fogger ]

        --layers [ bg, pg ]
        , flow down <| List.map text (List.take 3 state.log)
        ]



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


sidebar : GameModel.State -> Element
sidebar state =
    let
        x =
            5

        bar =
            flow down
                [ flow right [ state.player.avatar, text <| ": " ++ state.player.name ]
                , flow right [ text <| "Health: " ++ toString state.player.health ]
                , flow right [ text <| "Energy: " ++ toString state.player.energy ]
                , flow right [ text <| "Hunger: " ++ toString state.player.hunger ]
                , flow right [ text <| "Stealth: " ++ toString state.player.stealth ++ "%" ]
                , flow right [ text <| "Armor: " ++ toString state.player.armor ]
                , flow right [ text <| "Protection: " ++ toString state.player.protection ++ "%" ]
                , flow right [ text <| "Coordination: " ++ toString state.player.coordination ++ "%" ]
                , flow right [ text <| "Power: " ++ toString state.player.power ]
                , flow right [ text <| "Initiative: " ++ toString state.player.initiative ]
                ]
    in
    container (widthOf bar + 20) (heightOf bar) midTop bar


display : GameModel.State -> Element
display state =
    flow right [ sidebar state, mainScreen state ] |> color black


gridToHtmlList : Grid.Grid a -> List (Html msg)
gridToHtmlList grid =
    let
        lofls =
            Grid.toList grid

        funcListToString l =
            List.foldl (\x y -> y ++ " , " ++ toString x) "" l

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
                ++ toString state.player.location.x
                ++ " , "
                ++ toString state.player.location.y
                ++ " , has health = "
                ++ toString state.player.health
                ++ " , and energy = "
                ++ toString state.player.energy
            )
        ]


viewDebugEnemies : GameModel.State -> List (Html msg)
viewDebugEnemies state =
    let
        enemyToHtmlFunc enemy enemyId =
            Html.h2 []
                [ Html.text
                    ("enemy "
                        ++ toString enemyId
                        ++ " is in position "
                        ++ toString enemy.location.x
                        ++ " , "
                        ++ toString enemy.location.y
                        ++ " , has health = "
                        ++ toString enemy.health
                    )
                ]
    in
    Dict.map (\enid enemy -> enemyToHtmlFunc enemy enid) state.enemies
        |> Dict.values


view : GameModel.State -> Html GameUpdate.Msg
view model =
    Html.div []
        ([ display model
            |> Element.toHtml
         ]
            ++ [ viewDebugPlayer model ]
            --  ++ viewDebugEnemies model
            ++ viewDebugGrid model.level model
        )
