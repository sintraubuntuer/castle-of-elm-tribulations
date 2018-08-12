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
            --fillTunnelSidesWithWalls newGrid ltunnelrectangles
            --createWallBoundary newGrid
            newGrid
                |> installLeverNearCornerRoom1 lroomrectangles
                --|> fillTunnelSidesWithWalls ltunnelrectangles Nothing
                --|> fillTunnelSidesWithWalls lroomrectangles (Just "horizontal")
                --|> fillTunnelSidesWithWalls lroomrectangles (Just "vertical")
                |> createWallBoundaries (lroomrectangles ++ ltunnelrectangles)
    in
    ( newnewgrid, lroomrectangles, ltunnelrectangles, lunusedrandints )


fillTunnelSidesWithWalls : List GameModel.TunnelRectangle -> Maybe String -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
fillTunnelSidesWithWalls ltunnelrectangles mbHorizontalOrVerticalStr grid =
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
    case mbHorizontalOrVerticalStr of
        Nothing ->
            List.foldl (\tunnel gridacc -> checkAndFillNeighbours (getTunnelCellCoords tunnel) (getHorizontalOrVertical tunnel) gridacc) grid ltunnelrectangles

        Just horizontalOrVerticalStr ->
            List.foldl (\tunnel gridacc -> checkAndFillNeighbours (getTunnelCellCoords tunnel) horizontalOrVerticalStr gridacc) grid ltunnelrectangles


installLeverNearCornerRoom1 : List GameModel.RoomRectangle -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
installLeverNearCornerRoom1 lroomrectangles grid =
    let
        mbXYpos =
            case lroomrectangles |> List.head of
                Nothing ->
                    Nothing

                Just rrect ->
                    Just ( rrect.top_left_x + rrect.width - 1, rrect.top_left_y + rrect.height - 1 )
    in
    case mbXYpos of
        Nothing ->
            grid

        Just ( x, y ) ->
            Grid.set (Grid.Coordinate x y) (GameModel.Lever GameModel.defaultLeverInfo) grid


determineRectangularRegionBoundaries : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> String -> List Grid.Coordinate
determineRectangularRegionBoundaries rrect topBotLeftRightStr =
    if topBotLeftRightStr == "top" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x rrect.top_left_y)
    else if topBotLeftRightStr == "top-1" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y - 1))
    else if topBotLeftRightStr == "bottom" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y + rrect.height - 1))
    else if topBotLeftRightStr == "bottom+1" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y + rrect.height - 1 + 1))
    else if topBotLeftRightStr == "left" then
        List.range rrect.top_left_y (rrect.top_left_y + rrect.height - 1)
            |> List.map (\y -> Grid.Coordinate rrect.top_left_x y)
    else if topBotLeftRightStr == "left-1" then
        List.range rrect.top_left_y (rrect.top_left_y + rrect.height - 1)
            |> List.map (\y -> Grid.Coordinate (rrect.top_left_x - 1) y)
    else if topBotLeftRightStr == "right" then
        List.range rrect.top_left_y (rrect.top_left_y + rrect.height - 1)
            |> List.map (\y -> Grid.Coordinate (rrect.top_left_x + rrect.width - 1) y)
    else if topBotLeftRightStr == "right+1" then
        List.range rrect.top_left_y (rrect.top_left_y + rrect.height - 1)
            |> List.map (\y -> Grid.Coordinate (rrect.top_left_x + rrect.width - 1 + 1) y)
    else
        []


determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> String -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect topBotLeftRightStr grid =
    let
        lcoords =
            determineRectangularRegionBoundaries rrect topBotLeftRightStr
    in
    List.foldl
        (\cellcoords gridacc ->
            if Grid.get cellcoords gridacc == Just GameModel.NoTileYet then
                Grid.set cellcoords (GameModel.Wall GameModel.defaultWallInfo) gridacc
            else
                gridacc
        )
        grid
        lcoords


createWallBoundaries : List { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
createWallBoundaries lrrect grid =
    let
        fillBoundariesIfNecessary rrect grid_ =
            determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "top-1" grid_
                |> determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "bottom+1"
                |> determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "left-1"
                |> determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "right+1"
    in
    List.foldl (\rrect gridacc -> fillBoundariesIfNecessary rrect gridacc) grid lrrect


createWallBoundary : Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
createWallBoundary grid =
    let
        fillBoundary : Grid.Coordinate -> Grid.Grid GameModel.Tile -> Grid.Grid GameModel.Tile
        fillBoundary coord grid_ =
            let
                current_tile =
                    Grid.get coord grid_
                        |> Maybe.withDefault GameModel.NoTileYet

                left_neighbour_tile =
                    Grid.get (Grid.Coordinate (coord.x - 1) coord.y) grid_
                        |> Maybe.withDefault GameModel.NoTileYet

                right_neighbour_tile =
                    Grid.get (Grid.Coordinate (coord.x + 1) coord.y) grid_
                        |> Maybe.withDefault GameModel.NoTileYet

                top_neighbour_tile =
                    Grid.get (Grid.Coordinate coord.x (coord.y - 1)) grid_
                        |> Maybe.withDefault GameModel.NoTileYet

                bottom_neighbour_tile =
                    Grid.get (Grid.Coordinate coord.x (coord.y + 1)) grid_
                        |> Maybe.withDefault GameModel.NoTileYet
            in
            if GameModel.isNoTileYet current_tile then
                if
                    (GameModel.isNoTileYet left_neighbour_tile && GameModel.isFloor right_neighbour_tile)
                        || (GameModel.isNoTileYet right_neighbour_tile && GameModel.isFloor left_neighbour_tile)
                        || (GameModel.isNoTileYet top_neighbour_tile && GameModel.isFloor bottom_neighbour_tile)
                        || (GameModel.isNoTileYet bottom_neighbour_tile && GameModel.isFloor top_neighbour_tile)
                        || (GameModel.isFloor top_neighbour_tile && GameModel.isFloor bottom_neighbour_tile)
                        || (GameModel.isFloor left_neighbour_tile && GameModel.isFloor right_neighbour_tile)
                        || (GameModel.isWall top_neighbour_tile && GameModel.isFloor bottom_neighbour_tile)
                        || (GameModel.isFloor top_neighbour_tile && GameModel.isWall bottom_neighbour_tile)
                then
                    Grid.set coord (GameModel.Wall GameModel.defaultWallInfo) grid_
                else
                    grid_
            else
                grid_

        x_range =
            List.range 0 (grid.size.width - 1)

        y_range =
            List.range 0 (grid.size.height - 1)

        lcoords =
            List.concatMap (\x -> List.map (\y -> Grid.Coordinate x y) y_range) x_range

        final_grid =
            List.foldl (\coord gridacc -> fillBoundary coord gridacc) grid lcoords
    in
    final_grid


cellBelongsToMoreThanARectRegion : Grid.Coordinate -> List { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Bool
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


cellBelongsToARectRegion : Grid.Coordinate -> List { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Bool
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
                    if newRandom < 150 then
                        --createMbHorizontalAndMbVerticalTunnels roomrect1 roomrect2
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


createMbHorizontalAndMbVerticalTunnels : GameModel.RoomRectangle -> GameModel.RoomRectangle -> ( Maybe GameModel.TunnelRectangle, Maybe GameModel.TunnelRectangle )
createMbHorizontalAndMbVerticalTunnels roomrect1 roomrect2 =
    let
        horizontal_y =
            GameModel.getRoomCenterY roomrect1

        ( horizontal_start_x, horizontal_end_x, horizontal_tunnel_length_ ) =
            if GameModel.getRoomRightX roomrect1 <= GameModel.getRoomLeftX roomrect2 then
                ( GameModel.getRoomRightX roomrect1, GameModel.getRoomLeftX roomrect2, GameModel.getRoomLeftX roomrect2 - GameModel.getRoomRightX roomrect1 )
            else if GameModel.getRoomRightX roomrect1 > GameModel.getRoomLeftX roomrect2 && GameModel.getRoomRightX roomrect1 <= GameModel.getRoomCenterX roomrect2 then
                ( GameModel.getRoomRightX roomrect1, GameModel.getRoomCenterX roomrect2, GameModel.getRoomCenterX roomrect2 - GameModel.getRoomRightX roomrect1 )
            else if GameModel.getRoomRightX roomrect1 > GameModel.getRoomCenterX roomrect2 && GameModel.getRoomLeftX roomrect1 <= GameModel.getRoomCenterX roomrect2 then
                ( GameModel.getRoomCenterX roomrect2, GameModel.getRoomCenterX roomrect2, 0 )
                -- onlyvertical
            else if GameModel.getRoomLeftX roomrect1 > GameModel.getRoomCenterX roomrect2 && GameModel.getRoomLeftX roomrect1 < GameModel.getRoomRightX roomrect2 then
                ( GameModel.getRoomCenterX roomrect2, GameModel.getRoomLeftX roomrect1, GameModel.getRoomLeftX roomrect1 - GameModel.getRoomCenterX roomrect2 )
            else if GameModel.getRoomLeftX roomrect1 >= GameModel.getRoomRightX roomrect2 then
                ( GameModel.getRoomRightX roomrect2, GameModel.getRoomLeftX roomrect1, GameModel.getRoomLeftX roomrect1 - GameModel.getRoomRightX roomrect2 )
            else
                ( 0, 0, 0 )

        horizontal_tunnel_length =
            horizontal_end_x - horizontal_start_x + 1

        vertical_x =
            horizontal_end_x

        ( vertical_start_y, vertical_end_y ) =
            if horizontal_tunnel_length <= 1 then
                if GameModel.getRoomBottomY roomrect1 <= GameModel.getRoomTopY roomrect2 then
                    ( GameModel.getRoomBottomY roomrect1, GameModel.getRoomTopY roomrect2 )
                else if GameModel.getRoomBottomY roomrect1 > GameModel.getRoomTopY roomrect2 && GameModel.getRoomTopY roomrect1 <= GameModel.getRoomBottomY roomrect2 then
                    ( 0, 0 )
                else if GameModel.getRoomTopY roomrect1 > GameModel.getRoomBottomY roomrect2 then
                    ( GameModel.getRoomBottomY roomrect2, GameModel.getRoomTopY roomrect1 )
                else
                    ( 0, 0 )
            else if horizontal_tunnel_length > 1 then
                if GameModel.getRoomCenterX roomrect1 < GameModel.getRoomTopY roomrect2 then
                    ( GameModel.getRoomCenterX roomrect1, GameModel.getRoomTopY roomrect2 )
                else if GameModel.getRoomCenterX roomrect1 > GameModel.getRoomBottomY roomrect2 then
                    ( GameModel.getRoomBottomY roomrect2, GameModel.getRoomCenterX roomrect1 )
                else
                    ( 0, 0 )
            else
                ( 0, 0 )

        vertical_tunnel_height =
            vertical_end_y - vertical_start_y + 1

        mbHorizontalTunnel =
            if horizontal_end_x > horizontal_start_x then
                Just (GameModel.TunnelRectangle horizontal_start_x horizontal_y horizontal_tunnel_length 1)
            else
                Nothing

        mbVerticalTunnel =
            if vertical_end_y > vertical_start_y then
                Just (GameModel.TunnelRectangle vertical_x vertical_start_y 1 vertical_tunnel_height)
            else
                Nothing
    in
    ( mbHorizontalTunnel, mbVerticalTunnel )


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
    listDungeonRectangleToGridFunc lroomrects False grid
