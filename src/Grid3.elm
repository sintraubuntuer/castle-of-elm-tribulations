module Grid3 exposing
    ( Coordinate
    , Coordinate2
    , Grid
    , Size
    , fromList
    , get
    ,  getFloorGrid
       -- , getColumnWithDefault
       -- , setColumn

    , getFloorRow
    , getFloorRowOrEmptyList
    , getFloorRowWithDefault
    , getFloorSize
    , getFloorSubGrid
    , getGridBoundsToPlaceFightingCharacter
    , getGridBoundsToPlacePlayer
    , getSize
    , getTheLength
    , getWithDefault
    , inGrid
    , initialize
    , map
    , neighborhoodCalc
    , set
    , toCoordinates
    , toFloorCoordinates
    , toList
    )

import Array
import Dict exposing (Dict)
import Grid2
import List


type alias Grid a =
    { grid : Array.Array (Array.Array (Array.Array a))
    }


type alias SizeDict =
    Dict Int { width : Int, height : Int }


type alias Size =
    { width : Int
    , height : Int
    , nr_floors : Int
    }


type alias Size2 =
    { width : Int
    , height : Int
    }


type alias Coordinate =
    { x : Int
    , y : Int
    , z : Int
    }


type alias Coordinate2 =
    { x : Int
    , y : Int
    }


initialize : Size -> a -> Grid a
initialize ({ width, height, nr_floors } as size) a =
    let
        oneFloor =
            Array.repeat height << Array.repeat width <| a
    in
    Grid (Array.repeat nr_floors oneFloor)


getGridBoundsToPlacePlayer : Int -> Grid a -> { minX : Int, maxX : Int, minY : Int, maxY : Int }
getGridBoundsToPlacePlayer floorId grid =
    { minX = 1
    , maxX = getFloorSize floorId grid |> (\x -> x.width - 1)
    , minY = 1
    , maxY = getFloorSize floorId grid |> (\x -> x.height - 1)
    }


getGridBoundsToPlaceFightingCharacter : Int -> Grid a -> { minX : Int, maxX : Int, minY : Int, maxY : Int }
getGridBoundsToPlaceFightingCharacter floorId grid =
    { minX = 1
    , maxX = getFloorSize floorId grid |> (\x -> x.width - 1)
    , minY = 1
    , maxY = getFloorSize floorId grid |> (\x -> x.height - 1)
    }


toList : Grid a -> List (List (List a))
toList grid =
    let
        listOfFloorArrays =
            Array.toList grid.grid

        floorArrayToList fa =
            Array.toList fa
                |> List.map Array.toList
    in
    List.map floorArrayToList listOfFloorArrays


fromList : List (List (List a)) -> Grid a
fromList xs =
    let
        row x =
            Array.fromList x

        convertArrayOfFloorsToListOfFloors =
            xs
                |> List.map floorGrid
                |> Array.fromList

        floorGrid ll =
            Array.fromList <| List.map row ll

        nrFloors =
            List.length xs
    in
    Grid convertArrayOfFloorsToListOfFloors


getSize : Grid a -> SizeDict
getSize grid =
    let
        floorGridSize ll =
            ( getTheLength << List.head <| ll, List.length ll )

        sizeDict xs =
            xs
                |> List.indexedMap (\i ll -> ( i, floorGridSize ll ))
                |> List.foldl (\( i, ( w, h ) ) dictAcc -> Dict.insert i { width = w, height = h } dictAcc) Dict.empty
    in
    grid |> toList |> sizeDict


getFloorSize : Int -> Grid a -> Size2
getFloorSize floorId grid =
    Dict.get floorId (getSize grid)
        |> Maybe.withDefault { width = 0, height = 0 }


getTheLength : Maybe (List a) -> Int
getTheLength mbla =
    case mbla of
        Just l ->
            List.length l

        Nothing ->
            0


toFloorCoordinates : Int -> Grid a -> List Coordinate
toFloorCoordinates floorId gridder =
    let
        s : { width : Int, height : Int }
        s =
            getFloorSize floorId gridder

        xs : Int -> List ( Int, Int )
        xs y =
            List.map (\x -> ( x, y )) (List.range 0 (s.width - 1))

        pairs =
            List.concatMap xs (List.range 0 (s.height - 1))
    in
    List.map (\( x, y ) -> Coordinate x y floorId) pairs


toCoordinates : Grid a -> List Coordinate
toCoordinates gridder =
    let
        nrOfFloors =
            List.length (toList gridder)

        floorNrs =
            List.range 0 (nrOfFloors - 1)
    in
    List.concatMap (\floorNr -> toFloorCoordinates floorNr gridder) floorNrs


set : Coordinate -> a -> Grid a -> Grid a
set { x, y, z } a grid =
    case Array.get z grid.grid of
        Nothing ->
            grid

        Just floorGrid ->
            if x < 0 then
                grid

            else if y < 0 then
                grid
                --else if x >= (getFloorSize z grid |> (\rec -> rec.width)) then
                --    grid
                --else if y >= (getFloorSize z grid |> (\rec -> rec.height)) then
                --    grid

            else
                let
                    row =
                        case Array.get y floorGrid of
                            Just r ->
                                r

                            Nothing ->
                                Array.fromList []

                    row_ =
                        Array.set x a row

                    updatedFloorGrid =
                        Array.set y row_ floorGrid
                in
                { grid | grid = Array.set z updatedFloorGrid grid.grid }


get : Coordinate -> Grid a -> Maybe a
get { x, y, z } grid =
    case Array.get z grid.grid of
        Nothing ->
            Nothing

        Just floorGrid ->
            case Array.get y floorGrid of
                Nothing ->
                    Nothing

                Just row ->
                    Array.get x row


getWithDefault : a -> Coordinate -> Grid a -> a
getWithDefault default coordinate grid =
    case get coordinate grid of
        Nothing ->
            default

        Just a ->
            a


getFloorRow : Int -> Int -> Grid a -> List a
getFloorRow floorNr n grid =
    if n < 0 then
        []

    else if n >= (getFloorSize floorNr grid |> (\x -> x.height)) then
        []

    else
        getFloorRowOrEmptyList floorNr n grid


getFloorRowOrEmptyList : Int -> Int -> Grid a -> List a
getFloorRowOrEmptyList floorNr n grid =
    case Array.get floorNr grid.grid of
        Nothing ->
            []

        Just floorGrid ->
            case Array.get n floorGrid of
                Just r ->
                    r |> Array.toList

                Nothing ->
                    []


getFloorRowWithDefault : List a -> Int -> Int -> Grid a -> List a
getFloorRowWithDefault default floorNr n grid =
    let
        lrow =
            getFloorRow floorNr n grid
    in
    if List.length lrow == 0 then
        default

    else
        lrow


getFloorSubGrid : Int -> Int -> Int -> Int -> Int -> Grid a -> ( Grid2.Grid a, String )
getFloorSubGrid floorNr minCol maxCol minRow maxRow grid =
    case Array.get floorNr grid.grid of
        Nothing ->
            ( Grid2.fromList [ [] ], "" )

        Just floorGrid ->
            let
                grid_nr_rows =
                    List.length (Array.toList floorGrid)

                minCol_ =
                    max 0 minCol

                --maxCol_ =
                --    min maxCol (getFloorSize floorNr grid |> (\x -> x.width - 1))
                --(floorGrid.size.width - 1)
                --        |> max minCol_
                minRow_ =
                    max 0 minRow

                --maxRow_ =
                --    min maxRow (getFloorSize floorNr grid |> (\x -> x.height - 1))
                -- .size.height - 1)
                --        |> max minRow_
                x_range =
                    List.range minCol_ maxCol

                therows =
                    List.range minRow_ maxRow
                        |> List.map (\v -> getFloorRow floorNr v grid)

                txtmsg =
                    ("getSubGrid has been called with minCol " ++ String.fromInt minCol ++ " , with maxCol " ++ String.fromInt maxCol ++ " , with minRow " ++ String.fromInt minRow ++ " , with maxRow " ++ String.fromInt maxRow)
                        -- ++ ("getSubGrid has been called with minCol_ " ++ String.fromInt minCol_ ++ " , with maxCol_ " ++ String.fromInt maxCol_ ++ " , with minRow_ " ++ String.fromInt minRow_ ++ " , with maxRow_ " ++ String.fromInt maxRow_)
                        ++ (" rows of the subgrid has " ++ String.fromInt (List.length therows))
            in
            ( List.map (\lrow -> List.drop minCol_ lrow) therows
                |> List.map (\lrow -> List.take (maxCol - minCol + 1) lrow)
                |> Grid2.fromList
            , txtmsg
            )


getFloorGrid : Int -> Grid a -> Maybe (Grid2.Grid a)
getFloorGrid floorNr grid =
    let
        mbgrid2 =
            Array.get floorNr grid.grid
    in
    case mbgrid2 of
        Just grid2 ->
            Just <| Grid2.Grid grid2 (Grid2.getGridSize grid2)

        Nothing ->
            Nothing


getElemWithDefault : Int -> a -> List a -> a
getElemWithDefault y aval lelems =
    List.drop y lelems |> List.head |> Maybe.withDefault aval


inGrid : Coordinate -> Grid a -> Bool
inGrid { x, y, z } grid =
    let
        sDict =
            getSize grid
    in
    case Dict.get z sDict of
        Nothing ->
            False

        Just { width, height } ->
            if x < 0 then
                False

            else if x >= width then
                False

            else if y < 0 then
                False

            else if y >= height then
                False

            else
                True


map : (a -> b) -> Grid a -> Grid b
map f grid =
    let
        floorGrid floorNr =
            Array.get floorNr grid.grid

        updatedFloorgrid floorNr =
            case floorGrid floorNr of
                Just farr ->
                    Array.map (\row -> Array.map f row) farr

                Nothing ->
                    Grid2.fromList [ [] ]
                        |> .grid

        newGrid =
            Array.indexedMap (\floorNr fArr -> updatedFloorgrid floorNr) grid.grid
    in
    Grid newGrid


neighborhoodCalc : Int -> Coordinate -> List Coordinate
neighborhoodCalc d { x, y, z } =
    let
        linc =
            List.range -d d

        possible_new_x =
            List.map (\el -> el + x) linc

        possible_new_y =
            List.map (\el -> el + y) linc

        possible_end_pos =
            List.concatMap (\el -> List.map (\yel -> Coordinate el yel z) possible_new_y) possible_new_x
    in
    possible_end_pos
