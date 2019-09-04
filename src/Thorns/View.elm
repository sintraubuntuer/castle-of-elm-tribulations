module Thorns.View exposing (view)

import Beings.Beings as Beings
import Dict exposing (Dict)
import Grid
import Html exposing (Html, a, br, div, h1, h2, h3, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onMouseOut, onMouseOver)
import Thorns.ThornGrid as ThornGrid
import Thorns.Types as Types exposing (Model, Msg(..))


getImgBaseDir : Model -> String
getImgBaseDir model =
    model.imgBaseDir
        |> Maybe.withDefault "./img"


view : Model -> Html Msg
view model =
    let
        lcoords =
            Grid.toCoordinates model.gridInteractionOptions

        lrownrs =
            List.range 0 (model.gridInteractionOptions |> (\g -> g.size.height))

        lrows =
            List.map (\nr -> Grid.getRow nr model.gridInteractionOptions) lrownrs

        getAttr rownr colnr =
            if List.member (Grid.Coordinate colnr rownr) model.currentSegment then
                ( [ Attr.style "font-weight" "bold", Attr.style "text-decoration" "none !important" ]
                , [ Attr.style "color" "#DD0000", Attr.style "text-decoration" "none !important" ]
                )

            else
                ( [ Attr.style "font-weight" "regular" ]
                , [ Attr.style "color" "black" ]
                )

        opponentSpecies oppo =
            case oppo of
                Types.FightingCharacter fchar ->
                    fchar.species

                Types.Ochar ochar ->
                    ochar.species

        imgBaseDir =
            getImgBaseDir model
    in
    div [ Attr.align "center" ]
        [ div []
            [ h3 [] [ text "How about a nice game of thorns ?" ]
            , div []
                [ span [] [ Html.img [ Attr.width 64, Attr.height 64, Attr.src (imgBaseDir ++ "/pc/right.png") ] [] ]
                , case model.opponent of
                    Just oppon ->
                        span [] [ Html.img [ Attr.width 50, Attr.height 50, Attr.src ((imgBaseDir ++ "/characters/") ++ String.toLower (opponentSpecies oppon) ++ ".png") ] [] ]

                    Nothing ->
                        span [] []
                , div []
                    (List.indexedMap
                        (\rownr row ->
                            div [ Attr.style "padding" "1em", Attr.style "font-family" "monospace", Attr.style "font-size" "1em" ]
                                (rowToListStringIndex row
                                    |> List.map
                                        (\( str, colnr ) ->
                                            span
                                                (getAttr rownr colnr |> Tuple.first)
                                                [ a ((getAttr rownr colnr |> Tuple.second) ++ [ onMouseOut (MouseOut rownr colnr), onMouseOver (MouseOver rownr colnr), onClick (DoActivate rownr colnr) ]) [ text str ]
                                                ]
                                        )
                                )
                        )
                        lrows
                    )
                ]
            ]
        , viewSuggestion model
        , viewHealthReport model
        , br [] []
        , br [] []
        ]


viewSuggestion model =
    div [] [ model.helpStr |> Maybe.withDefault "" |> text ]


viewHealthReport : Model -> Html Msg
viewHealthReport model =
    div []
        [ div []
            [ text ("Your Health : " ++ String.fromInt model.player.health)
            , br [] []
            , text ("Your Mana : " ++ String.fromInt model.player.mana)
            ]
        , case model.opponent of
            Just (Types.FightingCharacter opponent) ->
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


rowToListStringIndex : List (Maybe ThornGrid.Thorn) -> List ( String, Int )
rowToListStringIndex lmbthorns =
    List.indexedMap (\i mbt -> ( mbthornToString mbt, i )) lmbthorns


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
