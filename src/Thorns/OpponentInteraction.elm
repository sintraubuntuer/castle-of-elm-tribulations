module Thorns.OpponentInteraction exposing
    ( do_activate_and_calc_opponents
    , getItemCombatOption
    , get_attack_value
    , get_defense_value
    , get_interaction_options
    )

import Beings exposing (Enemy, Player)
import Dict exposing (Dict)
import Grid
import Item exposing (Item)


type alias RandomVal =
    Int


type alias Thorn =
    Beings.OPPONENT_INTERACTION_OPTIONS


activate : Grid.Coordinate -> Maybe (List Grid.Coordinate) -> Grid.Grid (Maybe Thorn) -> ( Grid.Grid (Maybe Thorn), Maybe Thorn, Maybe Int )
activate coords mbSegment grid =
    let
        mb_thorn_opt =
            Grid.get coords grid
                |> Maybe.withDefault Nothing

        ( outGrid, mb_power ) =
            case mbSegment of
                Nothing ->
                    ( grid, Nothing )

                Just lcoords ->
                    if List.length lcoords < 2 then
                        ( grid, Nothing )

                    else
                        let
                            power_value =
                                List.length lcoords

                            newGrid =
                                List.foldl (\coord gridacc -> Grid.set coord Nothing gridacc) grid lcoords
                        in
                        ( newGrid, Just power_value )

        -- fall  ( outGrid   , lrands )
    in
    ( outGrid, mb_thorn_opt, mb_power )


do_activate_and_calc_opponents :
    Grid.Coordinate
    -> Maybe (List Grid.Coordinate)
    -> Grid.Grid (Maybe Thorn)
    -> Player
    -> Beings.Enemy
    -> { grid : Grid.Grid (Maybe Thorn), player : Player, opponent : Beings.Enemy, txtmsg : String }
do_activate_and_calc_opponents coords mbSegment grid player opponent =
    let
        ( finalGrid, mb_opt, mb_attack_power ) =
            activate coords mbSegment grid

        ( newPlayer, newOpponent, txtmsg ) =
            case mb_attack_power of
                Nothing ->
                    ( player, opponent, "" )

                Just val ->
                    case mb_opt of
                        Just opt ->
                            calcOpponentStatsAfterInteraction player opponent val opt

                        Nothing ->
                            ( player, opponent, "" )
    in
    { grid = finalGrid, player = newPlayer, opponent = newOpponent, txtmsg = txtmsg }


calcOpponentStatsAfterInteraction :
    { rec | mana : Int, health : Int, inventory : Dict String Item, power : Int, protection : Int, armor : Int }
    -> { rec2 | mana : Int, health : Int, inventory : Dict String Item, power : Int, protection : Int, armor : Int }
    -> Int
    -> Beings.OPPONENT_INTERACTION_OPTIONS
    -> ( { rec | mana : Int, health : Int, inventory : Dict String Item, power : Int, protection : Int, armor : Int }, { rec2 | mana : Int, health : Int, inventory : Dict String Item, power : Int, protection : Int, armor : Int }, String )
calcOpponentStatsAfterInteraction intervenient1 intervenient2 power opp_interaction_chosen_option =
    let
        get_defender_after_attack attacker_ defender_ power_ opp_interaction_chosen_option_ =
            let
                attack_power_value =
                    power_ + get_attack_value attacker_ opp_interaction_chosen_option_

                defense_power_value =
                    get_defense_value defender_ opp_interaction_chosen_option_

                damage =
                    (attack_power_value - defense_power_value)
                        |> (\x -> Basics.max 1 x)
                        |> Debug.log "damage caused is equal to : "

                defender_hp_after_attack =
                    Basics.max 0 (defender_.health - damage)
                        |> Debug.log "defender final health is : "

                new_defender =
                    { defender_ | health = defender_hp_after_attack }

                txt_msg =
                    ""
            in
            ( new_defender, txt_msg )

        ( intervenient1_f, intervenient2_f, ftxt_msg ) =
            case opp_interaction_chosen_option of
                Beings.CHICANE_ATTACK ->
                    let
                        ( def, txtm ) =
                            get_defender_after_attack intervenient1 intervenient2 power opp_interaction_chosen_option
                    in
                    ( intervenient1, def, "you initiate a chicane with your opponent. power of the chicane is : " ++ String.fromInt power ++ " , " ++ txtm )

                Beings.ENLIGHTENMENT_SPELL ->
                    if intervenient1.mana > power then
                        let
                            ( def, txtm ) =
                                get_defender_after_attack intervenient1 intervenient2 power opp_interaction_chosen_option
                        in
                        ( { intervenient1 | mana = intervenient1.mana - power }, def, "you enlighten your opponent with a light power of  : " ++ String.fromInt power ++ " , " ++ txtm )

                    else
                        ( intervenient1, intervenient2, "" )

                Beings.OPPONENT_CHICANE_ATTACK ->
                    let
                        ( def, txtm ) =
                            get_defender_after_attack intervenient2 intervenient1 power opp_interaction_chosen_option
                    in
                    ( def, intervenient2, "your opponent initiates a chicane with you . power of the chicane is : " ++ String.fromInt power ++ " , " ++ txtm )

                Beings.OPPONENT_ENLIGHTENMENT_SPELL ->
                    if intervenient2.mana < power then
                        ( intervenient1, intervenient2, "" )

                    else
                        let
                            ( def, txtm ) =
                                get_defender_after_attack intervenient2 intervenient1 power opp_interaction_chosen_option
                        in
                        ( def, { intervenient2 | mana = intervenient2.mana - power }, "your opponent tries to enlighten you with a light power of : " ++ String.fromInt power ++ " , " ++ txtm )
    in
    ( intervenient1_f, intervenient2_f, ftxt_msg )


get_attack_value : { rec | mana : Int, health : Int, inventory : Dict String Item, power : Int, protection : Int, armor : Int } -> Beings.OPPONENT_INTERACTION_OPTIONS -> Int
get_attack_value attacker opt =
    let
        modifier =
            0

        -- calc value using inventory items
    in
    attacker.power + modifier


get_defense_value : { rec | mana : Int, health : Int, inventory : Dict String Item, power : Int, protection : Int, armor : Int } -> Beings.OPPONENT_INTERACTION_OPTIONS -> Int
get_defense_value defender opt =
    let
        modifier =
            0

        -- calc value using inventory items
    in
    defender.protection + defender.armor + modifier


get_interaction_options : List RandomVal -> Player -> ( Maybe Beings.OPPONENT_INTERACTION_OPTIONS, List RandomVal )
get_interaction_options lrandints player =
    let
        lopt =
            --  [ Beings.CHICANE_ATTACK, Beings.ENLIGHTENMENT_SPELL, Beings.OPPONENT_CHICANE_ATTACK, Beings.OPPONENT_ENLIGHTENMENT_SPELL ]
            [ Beings.CHICANE_ATTACK, Beings.OPPONENT_CHICANE_ATTACK, Beings.CHICANE_ATTACK, Beings.OPPONENT_CHICANE_ATTACK, Beings.CHICANE_ATTACK, Beings.OPPONENT_CHICANE_ATTACK, Beings.ENLIGHTENMENT_SPELL, Beings.OPPONENT_ENLIGHTENMENT_SPELL, Beings.ENLIGHTENMENT_SPELL, Beings.OPPONENT_ENLIGHTENMENT_SPELL ]

        litems =
            Dict.values player.inventory

        chooseFrom : List Beings.OPPONENT_INTERACTION_OPTIONS
        chooseFrom =
            List.foldl (\item lacc -> getItemCombatOption item :: lacc) lopt litems

        indexNr : Int
        indexNr =
            lrandints |> List.head |> Maybe.map (\x -> toFloat x * toFloat (List.length chooseFrom) / 100.0 |> Basics.ceiling) |> Maybe.withDefault 1 |> (\x -> x - 1)
    in
    ( chooseFrom |> List.drop indexNr |> List.head, lrandints |> List.drop 1 )


getItemCombatOption : Item -> Beings.OPPONENT_INTERACTION_OPTIONS
getItemCombatOption item =
    Beings.ENLIGHTENMENT_SPELL
