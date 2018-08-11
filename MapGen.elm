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


getRandomIntBetweenValues : Int -> Int -> List Int -> ( Int, List Int )
getRandomIntBetweenValues minVal maxVal lrandomInts =
    let
        randInt1To100 =
            List.head lrandomInts |> Maybe.withDefault 1

        randInt =
            minVal + round (toFloat (randInt1To100 - 1) / 100.0 * toFloat (maxVal - minVal + 1))
    in
    ( randInt, List.drop 1 lrandomInts )


randomRoomGenerator : Int -> Int -> Int -> Int -> ( List GameModel.RoomRectangle, List Int ) -> ( List GameModel.RoomRectangle, List Int )
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
            GameModel.RoomRectangle room_topLeft_X room_topLeft_Y roomWidth roomHeight

        roomCheckIntersectsListRoomFunc : GameModel.RoomRectangle -> List GameModel.RoomRectangle -> Bool
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

        roomCheckIntersectsFunc : GameModel.RoomRectangle -> GameModel.RoomRectangle -> Bool
        roomCheckIntersectsFunc room1rectangle room2rectangle =
            let
                rect1_LeftX =
                    room1rectangle.top_left_x

                rect1_RightX =
                    room1rectangle.top_left_x + room1rectangle.width

                rect2_LeftX =
                    room2rectangle.top_left_x

                rect2_RightX =
                    room2rectangle.top_left_x + room2rectangle.width

                rect1_TopY =
                    room1rectangle.top_left_y

                rect1_BottomY =
                    room1rectangle.top_left_y + room1rectangle.height

                rect2_TopY =
                    room2rectangle.top_left_y

                rect2_BottomY =
                    room2rectangle.top_left_y + room2rectangle.height
            in
            if
                (rect1_RightX < rect2_LeftX || rect1_LeftX > rect2_RightX)
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
        let
            _ =
                Debug.log " adding new Room to Map : " newRoom
        in
        ( newRoom :: lroomrectangles, lrandIntsAfterXandY )


randomMapRoomRectanglesGenerator : Int -> Int -> Int -> Int -> Int -> List Int -> ( List GameModel.RoomRectangle, List Int )
randomMapRoomRectanglesGenerator totalwidth totalheight maxRooms roomMaxSize roomMinSize lrandomInts =
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


randomMapGeneratorWithRooms : Int -> Int -> Int -> Int -> Int -> List Int -> Grid.Grid GameModel.Tile -> ( Grid.Grid GameModel.Tile, List GameModel.RoomRectangle, List GameModel.TunnelRectangle, List Int )
randomMapGeneratorWithRooms totalwidth totalheight maxRooms roomMaxSize roomMinSize lrandomInts grid =
    let
        ( lroomrectangles, unused_lrandomints ) =
            randomMapRoomRectanglesGenerator totalwidth totalheight maxRooms roomMaxSize roomMinSize lrandomInts

        ( newGrid, ltunnelrectangles, lunusedrandints ) =
            grid
                |> listRoomRectangleToGridFunc lroomrectangles
                |> createHorizontalAndVerticalTunnels unused_lrandomints lroomrectangles

        newnewgrid =
            fillTunnelSidesWithWalls newGrid ltunnelrectangles
    in
    ( newnewgrid, lroomrectangles, ltunnelrectangles, lunusedrandints )


fillTunnelSidesWithWalls : Grid.Grid GameModel.Tile -> List GameModel.TunnelRectangle -> Grid.Grid GameModel.Tile
fillTunnelSidesWithWalls grid ltunnelrectangles =
    let
        checkAndFillNeighbours : List Grid.Coordinate -> String -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
        checkAndFillNeighbours ltunnelcellcoords strHorizontalOrVertical grid =
            let
                ( neighbourCell1, neighbourCell2 ) =
                    if strHorizontalOrVertical == "horizontal" then
                        ( List.map (\coords -> Grid.Coordinate coords.x (coords.y - 1)) ltunnelcellcoords
                        , List.map (\coords -> Grid.Coordinate coords.x (coords.y + 1)) ltunnelcellcoords
                        )
                    else
                        ( List.map (\coords -> Grid.Coordinate (coords.x - 1) coords.y) ltunnelcellcoords
                        , List.map (\coords -> Grid.Coordinate (coords.x + 1) coords.y) ltunnelcellcoords
                        )
            in
            List.foldl
                (\cellcoords gridacc ->
                    if Grid.get cellcoords gridacc == Just GameModel.NoTileYet then
                        Grid.set cellcoords (GameModel.Wall GameModel.defaultWallInfo) gridacc
                    else
                        gridacc
                )
                grid
                (neighbourCell1 ++ neighbourCell2)

        getTunnelCellCoords : GameModel.TunnelRectangle -> List Grid.Coordinate
        getTunnelCellCoords tunnel =
            let
                x_range =
                    List.range tunnel.top_left_x (tunnel.top_left_x + tunnel.width - 1)

                y_range =
                    List.range tunnel.top_left_y (tunnel.top_left_y + tunnel.height - 1)
            in
            List.concatMap (\x -> List.map (\y -> Grid.Coordinate x y) y_range) x_range

        getHorizontalOrVertical : GameModel.TunnelRectangle -> String
        getHorizontalOrVertical tunnel =
            if tunnel.width > tunnel.height then
                "horizontal"
            else
                "vertical"
    in
    List.foldl (\tunnel gridacc -> checkAndFillNeighbours (getTunnelCellCoords tunnel) (getHorizontalOrVertical tunnel) gridacc) grid ltunnelrectangles


cellBelongsToMoreThanARectRegion : Grid.Coordinate -> List GameModel.RoomRectangle -> Bool
cellBelongsToMoreThanARectRegion coord lrects =
    -- used to test if a cell belonging to a tunnel also is part of another tunnel or room
    let
        coordBelongsTorectRegion coord rrect =
            if coord.x >= rrect.top_left_x && coord.x <= rrect.top_left_x + rrect.width && coord.y >= rrect.top_left_y && coord.y <= rrect.top_left_y + rrect.height then
                1
            else
                0
    in
    if List.foldl (\roomrect intacc -> coordBelongsTorectRegion coord roomrect + intacc) 0 lrects >= 2 then
        True
    else
        False


cellBelongsToARectRegion : Grid.Coordinate -> List GameModel.RoomRectangle -> Bool
cellBelongsToARectRegion coord lrects =
    let
        coordBelongsTorectRegion coord rrect =
            if coord.x >= rrect.top_left_x && coord.x <= rrect.top_left_x + rrect.width && coord.y >= rrect.top_left_y && coord.y <= rrect.top_left_y + rrect.height then
                True
            else
                False
    in
    List.foldl (\roomrect bacc -> coordBelongsTorectRegion coord roomrect || bacc) False lrects


createHorizontalAndVerticalTunnels : List Int -> List GameModel.RoomRectangle -> Grid.Grid GameModel.Tile -> ( Grid.Grid GameModel.Tile, List GameModel.TunnelRectangle, List Int )
createHorizontalAndVerticalTunnels lrandomints lroomrectangles grid =
    let
        lrooms2 =
            List.drop 1 lroomrectangles

        ltupleRooms =
            List.map2 (,) lroomrectangles lrooms2

        tup_ltunnels_lrandints =
            ( [], lrandomints )

        createTunnels : GameModel.RoomRectangle -> GameModel.RoomRectangle -> ( List GameModel.TunnelRectangle, List Int ) -> ( List GameModel.TunnelRectangle, List Int )
        createTunnels roomrect1 roomrect2 tupacc =
            let
                ( ltunnels, lrints ) =
                    tupacc

                newRandom =
                    List.head lrints |> Maybe.withDefault 1

                ( mbnewTunnel1, mbnewTunnel2 ) =
                    if newRandom < 50 then
                        ( mbCreateHorizontalTunnel roomrect1 roomrect2
                        , mbCreateVerticalTunnel roomrect2 roomrect1
                        )
                    else
                        ( mbCreateVerticalTunnel roomrect1 roomrect2
                        , mbCreateHorizontalTunnel roomrect2 roomrect1
                        )

                new_ltunnels =
                    addMbElemToList mbnewTunnel2 ltunnels
                        |> addMbElemToList mbnewTunnel1
            in
            ( new_ltunnels, List.drop 1 lrints )

        ( ltunnels, lrandints ) =
            List.foldl (\( r1, r2 ) tupacc -> createTunnels r1 r2 tupacc) tup_ltunnels_lrandints ltupleRooms

        newGrid =
            listTunnelRectangleToGridFunc ltunnels grid
    in
    ( newGrid, ltunnels, lrandints )


addMbElemToList : Maybe a -> List a -> List a
addMbElemToList mba la =
    case mba of
        Nothing ->
            la

        Just a ->
            a :: la


mbCreateHorizontalTunnel : GameModel.RoomRectangle -> GameModel.RoomRectangle -> Maybe GameModel.TunnelRectangle
mbCreateHorizontalTunnel roomrect1 roomrect2 =
    let
        ( start_x, tunnel_length ) =
            if GameModel.getRoomRightX roomrect1 < GameModel.getRoomCenterX roomrect2 then
                ( GameModel.getRoomRightX roomrect1, GameModel.getRoomCenterX roomrect2 - GameModel.getRoomRightX roomrect1 + 1 )
            else if roomrect1.top_left_x > GameModel.getRoomCenterX roomrect2 then
                ( GameModel.getRoomCenterX roomrect2, roomrect1.top_left_x - GameModel.getRoomCenterX roomrect2 + 1 )
            else
                ( GameModel.getRoomCenterX roomrect1, 0 )
    in
    if tunnel_length > 0 then
        GameModel.TunnelRectangle start_x (GameModel.getRoomCenterY roomrect1) tunnel_length 1
            |> Debug.log "Creating Horizontal tunnel : "
            |> Just
    else
        Nothing


mbCreateVerticalTunnel : GameModel.RoomRectangle -> GameModel.RoomRectangle -> Maybe GameModel.TunnelRectangle
mbCreateVerticalTunnel roomrect1 roomrect2 =
    let
        ( start_y, tunnel_height ) =
            if GameModel.getRoomBottomY roomrect1 < GameModel.getRoomCenterY roomrect2 then
                ( GameModel.getRoomBottomY roomrect1, GameModel.getRoomCenterY roomrect2 - GameModel.getRoomBottomY roomrect1 + 1 )
            else if GameModel.getRoomTopY roomrect1 > GameModel.getRoomCenterY roomrect2 then
                ( GameModel.getRoomCenterY roomrect2, GameModel.getRoomTopY roomrect1 - GameModel.getRoomCenterY roomrect2 + 1 )
            else
                ( GameModel.getRoomCenterY roomrect1, 0 )
    in
    if tunnel_height > 0 then
        GameModel.TunnelRectangle (GameModel.getRoomCenterX roomrect1) start_y 1 tunnel_height
            |> Debug.log "Creating vertical tunnel : "
            |> Just
    else
        Nothing


listTunnelRectangleToGridFunc : List GameModel.TunnelRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
listTunnelRectangleToGridFunc ltunnels grid =
    -- for now , still have to write this
    listDungeonRectangleToGridFunc ltunnels False grid


dungeonRectangleToGridFunc : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Bool -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
dungeonRectangleToGridFunc roomrect useWalls grid =
    let
        left_x =
            roomrect.top_left_x

        right_x =
            roomrect.top_left_x + roomrect.width - 1

        top_y =
            roomrect.top_left_y

        bottom_y =
            roomrect.top_left_y + roomrect.height - 1

        lx =
            List.range left_x right_x

        ly =
            List.range top_y bottom_y

        generateTile : Int -> Int -> GameModel.Tile
        generateTile xval yval =
            if xval == left_x || xval == right_x || yval == bottom_y || yval == top_y then
                if useWalls then
                    GameModel.Wall GameModel.defaultWallInfo
                else
                    GameModel.Floor GameModel.defaultFloorInfo
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


listDungeonRectangleToGridFunc : List { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Bool -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
listDungeonRectangleToGridFunc lroomrects useWalls grid =
    List.foldl (\roomrect gridacc -> dungeonRectangleToGridFunc roomrect useWalls gridacc) grid lroomrects


listRoomRectangleToGridFunc : List GameModel.RoomRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
listRoomRectangleToGridFunc lroomrects grid =
    listDungeonRectangleToGridFunc lroomrects True grid



{-
   roomRectangleToGridFunc : GameModel.RoomRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
   roomRectangleToGridFunc roomrect grid =
       let
           left_x =
               roomrect.top_left_x

           right_x =
               roomrect.top_left_x + roomrect.width

           top_y =
               roomrect.top_left_y

           bottom_y =
               roomrect.top_left_y + roomrect.height

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


   listRoomRectangleToGridFunc : List GameModel.RoomRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
   listRoomRectangleToGridFunc lroomrects grid =
       List.foldl (\roomrect gridacc -> roomRectangleToGridFunc roomrect gridacc) grid lroomrects



      seed : Int
      seed =
          2013
-}
