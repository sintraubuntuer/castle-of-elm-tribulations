module Thorns.View exposing (view)

import Beings
import Dict exposing (Dict)
import Grid
import Html exposing (Html, a, br, div, h1, h2, h3, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Thorns.ThornGrid as ThornGrid
import Thorns.Types as Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    let
        lcoords =
            Grid.toCoordinates model.gridInteractionOptions

        lrownrs =
            List.range 0 (model.gridInteractionOptions |> (\g -> g.size.height))

        lrows =
            List.map (\nr -> Grid.getRow nr model.gridInteractionOptions) lrownrs
    in
    div [ Attr.align "center" ]
        [ div []
            [ h3 [] [ text "How about a nice game of thorns ?" ]
            , br [] []
            , div []
                (List.indexedMap
                    (\rownr row ->
                        div [ Attr.style "padding" "1em", Attr.style "font-family" "monospace", Attr.style "font-size" "2em" ]
                            (rowToListStringIndex row
                                |> List.map
                                    (\( str, colnr ) ->
                                        span
                                            (if ThornGrid.inList (Grid.Coordinate colnr rownr) model.currentSegment then
                                                [ Attr.style "color" "blue", Attr.style "font-weight" "bold" ]

                                             else
                                                [ Attr.style "color" "black", Attr.style "font-weight" "regular" ]
                                            )
                                            [ a [ onMouseOut (MouseOut rownr colnr), onMouseOver (MouseOver rownr colnr), onClick (DoActivate rownr colnr) ] [ text str ]
                                            ]
                                    )
                            )
                    )
                    lrows
                )
            ]
        , br [] []
        , br [] []
        , viewHealthReport model
        , br [] []
        , br [] []
        , if model.previousGrid /= Nothing then
            div [] [ a [ onClick UndoLastInteraction ] [ text "Undo" ] ]

          else
            div [] []
        ]


viewHealthReport : Model -> Html Msg
viewHealthReport model =
    div []
        [ div [] [ text ("Your Health : " ++ String.fromInt model.player.health) ]
        , case model.opponent of
            Just (Types.Enemy opponent) ->
                div []
                    [ br [] []
                    , text ("Your Opponent's health : " ++ String.fromInt opponent.health)
                    , br [] []
                    , if model.player.enlSpellEffect == Beings.IncreaseIndexOfLight || model.player.enlSpellEffect == Beings.DecreaseIndexOfLight then
                        text ("Your Opponent's Index of Light : " ++ String.fromInt opponent.indexOfLight)

                      else
                        text ""
                    ]

            Just (Types.Ochar opponent) ->
                div []
                    [ text ("Your Opponent's health : " ++ String.fromInt opponent.health)
                    , if model.player.enlSpellEffect == Beings.IncreaseIndexOfLight || model.player.enlSpellEffect == Beings.DecreaseIndexOfLight then
                        text ("Your Opponent's Index of Light : " ++ String.fromInt opponent.indexOfLight)

                      else
                        text ""
                    ]

            Nothing ->
                div [] []
        ]



--(List.map (\coord -> Grid.get coord model.gridInteractionOptions |> Maybe.withDefault Nothing |> mbthornToString |> text) lcoords)


rowToListStringIndex : List (Maybe ThornGrid.Thorn) -> List ( String, Int )
rowToListStringIndex lmbthorns =
    List.indexedMap (\i mbt -> ( mbthornToString mbt, i )) lmbthorns



--|> String.join " "


rowToString : List (Maybe ThornGrid.Thorn) -> String
rowToString lmbthorns =
    List.map (\mbt -> mbthornToString mbt) lmbthorns
        |> String.join " "


mbthornToString : Maybe ThornGrid.Thorn -> String
mbthornToString mbthorn =
    case mbthorn of
        Nothing ->
            "nothing"

        Just thorn ->
            ThornGrid.thornToString thorn
