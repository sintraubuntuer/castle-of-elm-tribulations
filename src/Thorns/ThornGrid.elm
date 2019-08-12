module Thorns.ThornGrid exposing
    ( Thorn
    , checkSegments
    , fall
    , findSegment
    , inList
    , randomizeGrid
    , thornToString
    )

import Beings exposing (OPPONENT_INTERACTION_OPTIONS(..))
import Dict exposing (Dict)
import Grid
import Thorns.OpponentInteraction as OpponentInteraction


type alias Thorn =
    --Int
    Beings.OPPONENT_INTERACTION_OPTIONS


thornToString : Thorn -> String
thornToString thorn =
    case thorn of
        CHICANE_ATTACK ->
            "+ "

        OPPONENT_CHICANE_ATTACK ->
            "- "

        ENLIGHTENMENT_SPELL ->
            "* "

        OPPONENT_ENLIGHTENMENT_SPELL ->
            "/ "


type alias RandomVal =
    Int


fall : ( Grid.Grid (Maybe Thorn), List RandomVal ) -> Beings.Player -> ( Grid.Grid (Maybe Thorn), List RandomVal )
fall ( grid, lrandval ) player =
    let
        l_column_numbers : List Int
        l_column_numbers =
            List.range 0 (grid.size.width - 1)

        getFallColumnNrAndSet : Int -> ( Grid.Grid (Maybe Thorn), List RandomVal ) -> ( Grid.Grid (Maybe Thorn), List RandomVal )
        getFallColumnNrAndSet cnr ( gridacc, lrandacc ) =
            let
                ( theColumn, lrandValsLeft ) =
                    Grid.getColumnWithDefault cnr Nothing gridacc
                        |> fallColumn lrandacc player
            in
            ( Grid.setColumn cnr Nothing theColumn gridacc, lrandValsLeft )
    in
    List.foldl (\cnr ( gridacc, lrandacc ) -> getFallColumnNrAndSet cnr ( gridacc, lrandacc )) ( grid, lrandval ) l_column_numbers


fallColumn : List RandomVal -> Beings.Player -> List (Maybe Thorn) -> ( List (Maybe Thorn), List RandomVal )
fallColumn lrands player lmbthorns =
    let
        filteredl =
            lmbthorns |> List.filter (\mbv -> mbv /= Nothing)

        --_ =
        --    Debug.log ("fall column was called with a column of " ++ (List.length lmbthorns |> String.fromInt) ++ " elements . After filtering out Nothing , number of remaining elements = ") (List.length filteredl)
        getNewl : ( List (Maybe Thorn), List RandomVal ) -> ( List (Maybe Thorn), List RandomVal )
        getNewl ( oldl, lrand ) =
            if List.length oldl < List.length lmbthorns then
                let
                    ( addValue, lrandremain ) =
                        getThornFromRandomValue lrand player

                    addValue_ =
                        addValue |> Maybe.withDefault Beings.ENLIGHTENMENT_SPELL
                in
                getNewl ( [ Just addValue_ ] ++ oldl, lrandremain )

            else
                ( oldl, lrand )
    in
    getNewl ( filteredl, lrands )


getThornFromRandomValue : List RandomVal -> Beings.Player -> ( Maybe Thorn, List RandomVal )
getThornFromRandomValue lrand player =
    -- for the time being
    --Beings.ENLIGHTENMENT_SPELL
    OpponentInteraction.get_interaction_options lrand player


extractSegment : Grid.Coordinate -> Grid.Grid (Maybe Thorn) -> ( List Grid.Coordinate, Grid.Grid (Maybe Thorn) )
extractSegment coords_ grid_ =
    let
        mbValAtCoords =
            Grid.get coords_ grid_
                |> Maybe.withDefault Nothing

        auxFuncExtract : Grid.Coordinate -> ( List Grid.Coordinate, Grid.Grid (Maybe Thorn) ) -> ( List Grid.Coordinate, Grid.Grid (Maybe Thorn) )
        auxFuncExtract coords ( lextracted, grid ) =
            if coords.x < 0 || coords.y < 0 || coords.x >= grid.size.width || coords.y >= grid.size.height || inList coords lextracted then
                ( lextracted, grid )

            else
                case Grid.get coords grid of
                    Nothing ->
                        ( lextracted, grid )

                    Just mbval ->
                        if mbval == Nothing || mbval /= mbValAtCoords then
                            ( lextracted, grid )

                        else
                            ( lextracted ++ [ coords ], Grid.set coords Nothing grid )
                                |> auxFuncExtract (Grid.Coordinate (coords.x + 1) coords.y)
                                |> auxFuncExtract (Grid.Coordinate (coords.x - 1) coords.y)
                                |> auxFuncExtract (Grid.Coordinate coords.x (coords.y + 1))
                                |> auxFuncExtract (Grid.Coordinate coords.x (coords.y - 1))
    in
    auxFuncExtract coords_ ( [], grid_ )


getAllSegments : Grid.Grid (Maybe Thorn) -> List (List Grid.Coordinate)
getAllSegments grid =
    let
        lcoords =
            Grid.toCoordinates grid
    in
    List.map (\coords -> Tuple.first (extractSegment coords grid)) lcoords
        |> List.sortWith listSegmentComparison


listSegmentComparison : List Grid.Coordinate -> List Grid.Coordinate -> Order
listSegmentComparison l1 l2 =
    if List.length l1 > List.length l2 then
        LT

    else if List.length l1 < List.length l2 then
        GT

    else
        EQ


findSegment : Grid.Coordinate -> Grid.Grid (Maybe Thorn) -> Maybe (List Grid.Coordinate)
findSegment coords grid =
    getAllSegments grid
        |> List.filter (\lcoords -> inList coords lcoords)
        |> List.head


inList : a -> List a -> Bool
inList a_val la =
    List.filter (\elem -> elem == a_val) la
        |> List.length
        |> (\x -> x > 0)


checkSegments : Beings.Player -> ( Grid.Grid (Maybe Thorn), List RandomVal ) -> ( Grid.Grid (Maybe Thorn), List RandomVal )
checkSegments player gridRandIntsTuple =
    let
        ( grid, lrandints ) =
            gridRandIntsTuple

        segments =
            getAllSegments grid

        len =
            List.head segments |> Maybe.withDefault [] |> List.length
    in
    if len >= 2 then
        ( grid, lrandints )

    else
        randomizeGrid grid lrandints player


randomizeGrid : Grid.Grid (Maybe Thorn) -> List RandomVal -> Beings.Player -> ( Grid.Grid (Maybe Thorn), List RandomVal )
randomizeGrid grid lrandints player =
    let
        lcoords =
            Grid.toCoordinates grid

        updateGridAndRandInts coord_ grid_ lrands player_ =
            let
                ( val, newl ) =
                    OpponentInteraction.get_interaction_options lrands player_
            in
            ( Grid.set coord_ val grid_, newl )

        ( newGrid, newlrandints ) =
            List.foldl (\coord ( gridacc, lrandintsacc ) -> updateGridAndRandInts coord gridacc lrandintsacc player) ( grid, lrandints ) lcoords
    in
    ( newGrid, newlrandints )
