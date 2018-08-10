module Grid exposing (..)

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


getGridBoundsToPlaceEnemy : Grid a -> { minX : Int, maxX : Int, minY : Int, maxY : Int }
getGridBoundsToPlaceEnemy grid =
    { minX = 1
    , maxX = grid.size.width - 1
    , minY = 1
    , maxY = grid.size.height - 1
    }


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
                --Array.getOrFail y grid.grid
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



{- }
   getOrFail : Coordinate -> Grid a -> a
   getOrFail { x, y } grid =
       let
           row =
               --Array.getOrFail y grid.grid
               case Array.get y grid.grid of
                   Just r ->
                       r

                   Nothing ->
                       []
       in
       --Array.getOrFail x row
       case Array.get y grid.grid of
           Just elem ->
               elem

           Nothing ->
               []
-}


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
    --Array.toList << Array.getOrFail n << .grid
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
                --|> List.map (\v -> getRow (grid_nr_rows - 1 - v) grid)
                |> List.map (\v -> getRow v grid)

        txtmsg =
            ("getSubGrid has been called with minCol " ++ toString minCol ++ " , with maxCol " ++ toString maxCol ++ " , with minRow " ++ toString minRow ++ " , with maxRow " ++ toString maxRow)
                ++ ("getSubGrid has been called with minCol_ " ++ toString minCol_ ++ " , with maxCol_ " ++ toString maxCol_ ++ " , with minRow_ " ++ toString minRow_ ++ " , with maxRow_ " ++ toString maxRow_)
                ++ (" rows of the subgrid has " ++ toString (List.length therows))
    in
    ( List.map (\lrow -> List.drop minCol_ lrow) therows
        --List.map (\lrow -> List.drop (grid.size.width - (maxCol_ + 1)) lrow) therows
        |> List.map (\lrow -> List.take (maxCol_ - minCol_ + 1) lrow)
        |> fromList
    , txtmsg
    )



{-
   getColumn : Int -> Grid a -> Maybe (List a)
   getColumn n grid =
       if n < 0 then
           Nothing
       else if n >= grid.size.width then
           Nothing
       else
           Just << getColumnOrFail n <| grid


   getColumnOrFail : Int -> Grid a -> List a
   getColumnOrFail n grid =
       --List.map (Array.getOrFail n) << Array.toList << .grid
       --getElem arr = case
       --lgrid = Array.toList grid.grid
       toCoordinates grid
           |> List.filter (\( x, y ) -> x == n)
           |> List.map (\( x, y ) -> getOrElse GameModel.NoTileYet (Coordinate x y) grid)
           |> List.filter (\x -> x /= GameModel.NoTileYet)


   getColumnOrElse : List a -> Int -> Grid a -> List a
   getColumnOrElse default n grid =
       case getColumn n grid of
           Nothing ->
               default

           Just column ->
               column
-}


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
    { grid | grid = grid_ }


neighborhoodCalc : Int -> Coordinate -> List Coordinate
neighborhoodCalc d { x, y } =
    let
        linc =
            --[ 1, 0, -1 ]
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
