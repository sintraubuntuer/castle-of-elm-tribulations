module MapGen exposing (..)

--import Generator
--import Generator.Standard
--import Graphics.Input as Input
--import GameView

import GameModel
import Grid


getNeighborsOrElse : a -> Grid.Grid a -> Grid.Coordinate -> List a
getNeighborsOrElse x grid coord =
    List.map (\c -> Grid.getWithDefault x c grid) <| Grid.neighborhoodCalc 1 coord


getNeighborsOrElse2 : a -> Grid.Grid a -> Grid.Coordinate -> List a
getNeighborsOrElse2 x grid coord =
    List.map (\c -> Grid.getWithDefault x c grid) <| Grid.neighborhoodCalc 2 coord


getNeighbors : Grid.Grid GameModel.Tile -> Grid.Coordinate -> List GameModel.Tile
getNeighbors =
    getNeighborsOrElse (GameModel.Wall GameModel.defaultWallInfo)


getNeighbors2 : Grid.Grid GameModel.Tile -> Grid.Coordinate -> List GameModel.Tile
getNeighbors2 =
    getNeighborsOrElse2 (GameModel.Wall GameModel.defaultWallInfo)


numberOfWalls : Grid.Grid GameModel.Tile -> Grid.Coordinate -> Int
numberOfWalls grid coord =
    getNeighbors grid coord
        |> List.filter
            (\t ->
                case t of
                    GameModel.Wall _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


numberOfWalls2 : Grid.Grid GameModel.Tile -> Grid.Coordinate -> Int
numberOfWalls2 grid coord =
    getNeighbors2 grid coord
        |> List.filter
            (\t ->
                case t of
                    GameModel.Wall _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


randomTile : Float -> GameModel.Tile
randomTile rfloat =
    let
        tile =
            --if rfloat < 0.4 then
            if rfloat < 0.4 then
                GameModel.Wall GameModel.defaultWallInfo
            else
                GameModel.Floor GameModel.defaultFloorInfo
    in
    tile


randomMap : ( Int, Int ) -> List Float -> Grid.Grid GameModel.Tile
randomMap ( w, h ) lfloats =
    let
        -- lfloats should have w*h elements
        initialGrid =
            Grid.initialize { width = w, height = h } GameModel.NoTileYet

        lcoordinates =
            Grid.toCoordinates initialGrid

        lcoordsFloats =
            List.map2 (\gcoord rfl -> ( gcoord, rfl )) lcoordinates lfloats
    in
    List.foldl (\( gcoord, rfl ) accy -> Grid.set gcoord (randomTile rfl) accy) initialGrid lcoordsFloats


iterate : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
iterate grid =
    let
        coords =
            Grid.toCoordinates grid

        x =
            List.map
                (\coord ->
                    ( coord
                    , if numberOfWalls grid coord >= 5 then
                        GameModel.Wall GameModel.defaultWallInfo
                      else
                        GameModel.Floor GameModel.defaultFloorInfo
                    )
                )
                coords
    in
    List.foldl (\( coord, a ) grid -> Grid.set coord a grid) grid x


iterate2 : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
iterate2 grid =
    let
        coords =
            Grid.toCoordinates grid

        rule coord =
            if numberOfWalls grid coord >= 5 then
                GameModel.Wall GameModel.defaultWallInfo
            else if numberOfWalls2 grid coord <= 2 then
                GameModel.Wall GameModel.defaultWallInfo
            else
                GameModel.Floor GameModel.defaultFloorInfo

        x =
            List.map (\coord -> ( coord, rule coord )) coords
    in
    List.foldl (\( coord, a ) grid -> Grid.set coord a grid) grid x


randomCave : ( Int, Int ) -> List Float -> Grid.Grid GameModel.Tile
randomCave ( w, h ) lfloats =
    let
        -- lfloats should have w*h elements
        bedrock =
            randomMap ( w, h ) lfloats
    in
    bedrock
        |> iterate2
        |> iterate2
        |> iterate2
        |> iterate2
        |> iterate
        |> iterate
        |> iterate


type alias RoomRectangle =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    }


getRandomIntBetweenValues : Int -> Int -> List Int -> ( Int, List Int )
getRandomIntBetweenValues minVal maxVal lrandomInts =
    let
        randInt1To100 =
            List.head lrandomInts |> Maybe.withDefault 1

        randInt =
            minVal + round (toFloat (randInt1To100 - 1) / 100.0) * (maxVal - minVal + 1)
    in
    ( randInt, List.drop 1 lrandomInts )


randomRoomGenerator : Int -> Int -> Int -> Int -> ( List RoomRectangle, List Int ) -> ( List RoomRectangle, List Int )
randomRoomGenerator totalwidth totalheight roomMaxSize roomMinSize ( lroomrectangles, lrandomInts ) =
    let
        ( roomWidth, lrandIntsAfterWidth ) =
            getRandomIntBetweenValues roomMinSize roomMaxSize lrandomInts

        ( roomHeight, lrandIntsAfterWidthHeight ) =
            getRandomIntBetweenValues roomMinSize roomMaxSize lrandIntsAfterWidth

        ( room_topLeft_X, lrandIntsAfterX ) =
            getRandomIntBetweenValues 0 (totalwidth - roomWidth - 1) lrandIntsAfterWidthHeight

        ( room_topLeft_Y, lrandIntsAfterXandY ) =
            getRandomIntBetweenValues 0 (totalheight - roomHeight - 1) lrandIntsAfterX

        newRoom =
            RoomRectangle room_topLeft_X room_topLeft_Y roomWidth roomHeight

        roomCheckIntersectsListRoomFunc : RoomRectangle -> List RoomRectangle -> Bool
        roomCheckIntersectsListRoomFunc room1rectangle lroomrectangles =
            List.foldl
                (\rrect boolacc ->
                    if roomCheckIntersectsFunc room1rectangle rrect then
                        True
                    else
                        boolacc
                )
                False
                lroomrectangles

        roomCheckIntersectsFunc : RoomRectangle -> RoomRectangle -> Bool
        roomCheckIntersectsFunc room1rectangle room2rectangle =
            let
                rect1_LeftX =
                    room1rectangle.x

                rect1_RightX =
                    room1rectangle.x + room1rectangle.width

                rect2_LeftX =
                    room2rectangle.x

                rect2_RightX =
                    room2rectangle.x + room2rectangle.width

                rect1_TopY =
                    room1rectangle.y

                rect1_BottomY =
                    room1rectangle.y + room1rectangle.height

                rect2_TopY =
                    room2rectangle.y

                rect2_BottomY =
                    room2rectangle.y + room2rectangle.height
            in
            if
                (rect1_RightX < rect2_RightX || rect1_LeftX > rect2_RightX)
                    || (rect1_TopY > rect2_BottomY)
                    || (rect1_BottomY < rect2_TopY)
            then
                False
            else
                True
    in
    if roomCheckIntersectsListRoomFunc newRoom lroomrectangles then
        ( lroomrectangles, lrandIntsAfterXandY )
    else
        ( newRoom :: lroomrectangles, lrandIntsAfterXandY )


randomMapGeneratorWithRooms : Int -> Int -> Int -> Int -> Int -> List Int -> List RoomRectangle
randomMapGeneratorWithRooms totalwidth totalheight maxRooms roomMaxSize roomMinSize lrandomInts =
    let
        lroomnrs =
            List.range 1 maxRooms

        lroomrectanglesRandInts =
            ( []
            , lrandomInts
            )

        new_lroomrectanglesRandInts =
            List.foldl (\roomnr tupAcc -> randomRoomGenerator totalwidth totalheight roomMaxSize roomMinSize tupAcc) lroomrectanglesRandInts lroomnrs
    in
    new_lroomrectanglesRandInts
        |> Tuple.first


roomRectangleToGridFunc : RoomRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
roomRectangleToGridFunc roomrect grid =
    let
        left_x =
            roomrect.x

        right_x =
            roomrect.x + roomrect.width

        top_y =
            roomrect.y

        bottom_y =
            roomrect.y + roomrect.height

        lx =
            List.range left_x right_x

        ly =
            List.range top_y bottom_y

        generateTile : Int -> Int -> GameModel.Tile
        generateTile xval yval =
            if xval == left_x || xval == right_x || yval == bottom_y || yval == top_y then
                GameModel.Wall GameModel.defaultWallInfo
            else
                GameModel.Floor GameModel.defaultFloorInfo

        --generateTileAndAddItToGrid : Int -> Int -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
        --generateTileAndAddItToGrid xval yval grid =
        --  generateTile xval yval
        --      |> (\tile -> Grid.set (Grid.Coordinate xval yval) tile grid)
        ltiles =
            List.concatMap (\xval -> List.map (\yval -> ( xval, yval, generateTile xval yval )) ly) lx

        new_grid =
            ltiles
                |> List.foldl (\( xval, yval, tile ) gridacc -> Grid.set (Grid.Coordinate xval yval) tile gridacc) grid
    in
    new_grid


listRoomRectangleToGridFunc : List RoomRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
listRoomRectangleToGridFunc lroomrects grid =
    List.foldl (\roomrect gridacc -> roomRectangleToGridFunc roomrect gridacc) grid lroomrects



{- }

   seed : Int
   seed =
       2013
-}
