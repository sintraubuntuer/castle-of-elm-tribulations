module Grid2 exposing
    ( Coordinate
    , Grid
    , Size
    , fromList
    , get
    , getColumnWithDefault
    , getGridBoundsToPlaceFightingCharacter
    , getGridBoundsToPlacePlayer
    , getGridSize
    , getRow
    , getRowOrEmptyList
    , getRowWithDefault
    , getSubGrid
    , getTheLength
    , getWithDefault
    , inGrid
    , initialize
    , map
    , neighborhoodCalc
    , set
    , setColumn
    , toCoordinates
    , toList
    )

import Array
import List


type alias Grid a =
    { grid : Array.Array (Array.Array a)
    , size : Size
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias Coordinate =
    { x : Int
    , y : Int
    }


initialize : Size -> a -> Grid a
initialize ({ width, height } as size) a =
    Grid (Array.repeat height << Array.repeat width <| a) size


getGridBoundsToPlacePlayer : Grid a -> { minX : Int, maxX : Int, minY : Int, maxY : Int }
getGridBoundsToPlacePlayer grid =
    { minX = 1
    , maxX = grid.size.width - 1
    , minY = 1
    , maxY = grid.size.height - 1
    }


getGridBoundsToPlaceFightingCharacter : Grid a -> { minX : Int, maxX : Int, minY : Int, maxY : Int }
getGridBoundsToPlaceFightingCharacter grid =
    { minX = 1
    , maxX = grid.size.width - 1
    , minY = 1
    , maxY = grid.size.height - 1
    }


getGridSize : Array.Array (Array.Array a) -> Size
getGridSize grid =
    let
        xs =
            toList (Grid grid (Size 0 0))
    in
    Size (getTheLength << List.head <| xs) (List.length xs)


toList : Grid a -> List (List a)
toList =
    List.map Array.toList << Array.toList << .grid


fromList : List (List a) -> Grid a
fromList xs =
    let
        row x =
            Array.fromList x

        grid =
            Array.fromList <| List.map row xs
    in
    Grid grid <| Size (getTheLength << List.head <| xs) (List.length xs)


getTheLength : Maybe (List a) -> Int
getTheLength mbla =
    case mbla of
        Just l ->
            List.length l

        Nothing ->
            0


toCoordinates : Grid a -> List Coordinate
toCoordinates gridder =
    let
        s : Size
        s =
            gridder.size

        xs : Int -> List ( Int, Int )
        xs y =
            List.map (\x -> ( x, y )) (List.range 0 (s.width - 1))

        pairs =
            List.concatMap xs (List.range 0 (s.height - 1))
    in
    List.map (\( x, y ) -> Coordinate x y) pairs


set : Coordinate -> a -> Grid a -> Grid a
set { x, y } a grid =
    if x < 0 then
        grid

    else if y < 0 then
        grid

    else if x >= grid.size.width then
        grid

    else if y >= grid.size.height then
        grid

    else
        let
            row =
                case Array.get y grid.grid of
                    Just r ->
                        r

                    Nothing ->
                        Array.fromList []

            row_ =
                Array.set x a row
        in
        { grid | grid = Array.set y row_ grid.grid }


get : Coordinate -> Grid a -> Maybe a
get { x, y } grid =
    case Array.get y grid.grid of
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


getRow : Int -> Grid a -> List a
getRow n grid =
    if n < 0 then
        []

    else if n >= grid.size.height then
        []

    else
        getRowOrEmptyList n grid


getRowOrEmptyList : Int -> Grid a -> List a
getRowOrEmptyList n grid =
    case Array.get n grid.grid of
        Just r ->
            r |> Array.toList

        Nothing ->
            []


getRowWithDefault : List a -> Int -> Grid a -> List a
getRowWithDefault default n grid =
    let
        lrow =
            getRow n grid
    in
    if List.length lrow == 0 then
        default

    else
        lrow


getSubGrid : Int -> Int -> Int -> Int -> Grid a -> ( Grid a, String )
getSubGrid minCol maxCol minRow maxRow grid =
    let
        grid_nr_rows =
            List.length (grid |> toList)

        minCol_ =
            max 0 minCol

        maxCol_ =
            min maxCol (grid.size.width - 1)
                |> max minCol_

        minRow_ =
            max 0 minRow

        maxRow_ =
            min maxRow (grid.size.height - 1)
                |> max minRow_

        x_range =
            List.range minCol_ maxCol_

        therows =
            List.range minRow_ maxRow_
                |> List.map (\v -> getRow v grid)

        txtmsg =
            ("getSubGrid has been called with minCol " ++ String.fromInt minCol ++ " , with maxCol " ++ String.fromInt maxCol ++ " , with minRow " ++ String.fromInt minRow ++ " , with maxRow " ++ String.fromInt maxRow)
                ++ ("getSubGrid has been called with minCol_ " ++ String.fromInt minCol_ ++ " , with maxCol_ " ++ String.fromInt maxCol_ ++ " , with minRow_ " ++ String.fromInt minRow_ ++ " , with maxRow_ " ++ String.fromInt maxRow_)
                ++ (" rows of the subgrid has " ++ String.fromInt (List.length therows))
    in
    ( List.map (\lrow -> List.drop minCol_ lrow) therows
        |> List.map (\lrow -> List.take (maxCol_ - minCol_ + 1) lrow)
        |> fromList
    , txtmsg
    )


getColumnWithDefault : Int -> a -> Grid a -> List a
getColumnWithDefault n a_val grid =
    if n < 0 then
        []

    else if n >= grid.size.width then
        []

    else
        List.range 0 (grid.size.height - 1)
            |> List.map (\rownr -> Coordinate n rownr)
            |> List.map (\coord -> get coord grid |> Maybe.withDefault a_val)


setColumn : Int -> a -> List a -> Grid a -> Grid a
setColumn n adef lelems grid =
    --check that column has same nr of elements than the others
    if List.length lelems /= grid.size.height then
        grid

    else
        toCoordinates grid
            |> List.filter (\coords -> coords.x == n)
            |> List.foldl (\coords gridacc -> set coords (getElemWithDefault coords.y adef lelems) gridacc) grid


getElemWithDefault : Int -> a -> List a -> a
getElemWithDefault y aval lelems =
    List.drop y lelems |> List.head |> Maybe.withDefault aval


inGrid : Coordinate -> Grid a -> Bool
inGrid { x, y } grid =
    let
        { height, width } =
            grid.size
    in
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
        grid_ =
            Array.map (\row -> Array.map f row) grid.grid
    in
    Grid grid_ grid.size


neighborhoodCalc : Int -> Coordinate -> List Coordinate
neighborhoodCalc d { x, y } =
    let
        linc =
            List.range -d d

        possible_new_x =
            List.map (\el -> el + x) linc

        possible_new_y =
            List.map (\el -> el + y) linc

        possible_end_pos =
            List.concatMap (\el -> List.map (\yel -> Coordinate el yel) possible_new_y) possible_new_x
    in
    possible_end_pos



{-
   neighborhood : Coordinate -> List Coordinate
   neighborhood { x, y } =
       List.map (\( a, b ) -> Coordinate a b)
           [ ( x - 1, y - 1 )
           , ( x, y - 1 )
           , ( x + 1, y - 1 )
           , ( x - 1, y )
           , ( x, y )
           , ( x + 1, y )
           , ( x - 1, y + 1 )
           , ( x, y + 1 )
           , ( x + 1, y + 1 )
           ]


   neighborhood2 : Coordinate -> List Coordinate
   neighborhood2 { x, y } =
       List.map (\( c, b ) -> Coordinate c b)
           [ ( x - 2, y - 2 )
           , ( x - 1, y - 2 )
           , ( x, y - 2 )
           , ( x + 1, y - 2 )
           , ( x + 2, y - 2 )
           , ( x - 2, y - 1 )
           , ( x - 1, y - 1 )
           , ( x, y - 1 )
           , ( x + 1, y - 1 )
           , ( x + 2, y - 1 )
           , ( x - 2, y )
           , ( x - 1, y )
           , ( x, y )
           , ( x + 1, y )
           , ( x + 2, y )
           , ( x - 2, y + 1 )
           , ( x - 1, y + 1 )
           , ( x, y + 1 )
           , ( x + 1, y + 1 )
           , ( x + 2, y + 1 )
           , ( x - 2, y + 2 )
           , ( x - 1, y + 2 )
           , ( x, y + 2 )
           , ( x + 1, y + 2 )
           , ( x + 2, y + 2 )
           ]
-}
