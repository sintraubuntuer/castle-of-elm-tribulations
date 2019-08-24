module MapGen exposing
    ( addMbElemToList
    , cellBelongsToARectRegion
    , cellBelongsToMoreThanARectRegion
    , correctSomeWallCorners
    , createHorizontalAndVerticalTunnels
    , createWallBoundaries
    , determineCornerAndInstallLever
    , determineLeverNearCornerRoom1Coords
    , determineRectangularRegionBoundaries
    , determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet
    , dungeonRectangleToGridFunc
    , getNeighbors
    , getNeighbors2
    , getNeighborsOrElse
    , getNeighborsOrElse2
    , getRandomIntBetweenValues
    , getRectangularRegionCellCoordinates
    , getTunnelFromCellCoordinates
    , installLeversInCoords
    , iterate
    , iterate2
    , listDungeonRectangleToGridFunc
    , listRoomRectangleToGridFunc
    , listTunnelRectangleToGridFunc
    , listTunnelRectangleWithOptionsToGridFunc
    , mbCreateHorizontalTunnel
    , mbCreateVerticalTunnel
    , numberOfWalls
    , numberOfWalls2
    , randomCave
    , randomMap
    , randomMapGeneratorWithRooms
    , randomMapRoomRectanglesGenerator
    , randomRoomGenerator
    , randomTile
    , transformFloorToWallOnDisplayBoundaries
    )

--import Generator
--import Generator.Standard
--import Graphics.Input as Input
--import GameView

import GameModel
import GameSimulation
import Grid
import Tile exposing (Tile(..))


getNeighborsOrElse : a -> Grid.Grid a -> Grid.Coordinate -> List a
getNeighborsOrElse x grid coord =
    List.map (\c -> Grid.getWithDefault x c grid) <| Grid.neighborhoodCalc 1 coord


getNeighborsOrElse2 : a -> Grid.Grid a -> Grid.Coordinate -> List a
getNeighborsOrElse2 x grid coord =
    List.map (\c -> Grid.getWithDefault x c grid) <| Grid.neighborhoodCalc 2 coord


getNeighbors : Grid.Grid Tile -> Grid.Coordinate -> List Tile
getNeighbors =
    getNeighborsOrElse (Tile.Wall GameModel.defaultWallInfo)


getNeighbors2 : Grid.Grid Tile -> Grid.Coordinate -> List Tile
getNeighbors2 =
    getNeighborsOrElse2 (Tile.Wall GameModel.defaultWallInfo)


numberOfWalls : Grid.Grid Tile -> Grid.Coordinate -> Int
numberOfWalls grid coord =
    getNeighbors grid coord
        |> List.filter
            (\t ->
                case t of
                    Tile.Wall _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


numberOfWalls2 : Grid.Grid Tile -> Grid.Coordinate -> Int
numberOfWalls2 grid coord =
    getNeighbors2 grid coord
        |> List.filter
            (\t ->
                case t of
                    Tile.Wall _ ->
                        True

                    _ ->
                        False
            )
        |> List.length


randomTile : Float -> Tile
randomTile rfloat =
    let
        tile =
            --if rfloat < 0.4 then
            if rfloat < 0.4 then
                Tile.Wall GameModel.defaultWallInfo

            else
                Tile.Floor GameModel.defaultFloorInfo
    in
    tile


randomMap : ( Int, Int ) -> List Float -> Grid.Grid Tile
randomMap ( w, h ) lfloats =
    let
        -- lfloats should have w*h elements
        initialGrid =
            Grid.initialize { width = w, height = h } Tile.NoTileYet

        lcoordinates =
            Grid.toCoordinates initialGrid

        lcoordsFloats =
            List.map2 (\gcoord rfl -> ( gcoord, rfl )) lcoordinates lfloats
    in
    List.foldl (\( gcoord, rfl ) accy -> Grid.set gcoord (randomTile rfl) accy) initialGrid lcoordsFloats


iterate : Grid.Grid Tile -> Grid.Grid Tile
iterate grid =
    let
        coords =
            Grid.toCoordinates grid

        x =
            List.map
                (\coord ->
                    ( coord
                    , if numberOfWalls grid coord >= 5 then
                        Tile.Wall GameModel.defaultWallInfo

                      else
                        Tile.Floor GameModel.defaultFloorInfo
                    )
                )
                coords
    in
    List.foldl (\( coord, a ) grid_ -> Grid.set coord a grid_) grid x


iterate2 : Grid.Grid Tile -> Grid.Grid Tile
iterate2 grid =
    let
        coords =
            Grid.toCoordinates grid

        rule coord =
            if numberOfWalls grid coord >= 5 then
                Tile.Wall GameModel.defaultWallInfo

            else if numberOfWalls2 grid coord <= 2 then
                Tile.Wall GameModel.defaultWallInfo

            else
                Tile.Floor GameModel.defaultFloorInfo

        x =
            List.map (\coord -> ( coord, rule coord )) coords
    in
    List.foldl (\( coord, a ) grid_ -> Grid.set coord a grid_) grid x


randomCave : ( Int, Int ) -> List Float -> Grid.Grid Tile
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
        roomCheckIntersectsListRoomFunc room1rectangle lroomrectangles_ =
            List.foldl
                (\rrect boolacc ->
                    if roomCheckIntersectsFunc room1rectangle rrect then
                        True

                    else
                        boolacc
                )
                False
                lroomrectangles_

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
        --let
        --    _ =
        --        Debug.log " adding new Room to Map : " newRoom
        --in
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


randomMapGeneratorWithRooms : Int -> Int -> Int -> Int -> Int -> List Int -> Grid.Grid Tile -> { tileGrid : Grid.Grid Tile, lroomRectangles : List GameModel.RoomRectangle, ltunnelRectangles : List GameModel.TunnelRectangle, unusedRandoms : List Int }
randomMapGeneratorWithRooms totalwidth totalheight maxRooms roomMaxSize roomMinSize lrandomInts grid =
    let
        ( lroomrectangles, unused_lrandomints ) =
            randomMapRoomRectanglesGenerator totalwidth totalheight maxRooms roomMaxSize roomMinSize lrandomInts

        ( newGrid, ltunnelrectangles, lunusedrandints ) =
            grid
                |> listRoomRectangleToGridFunc lroomrectangles
                |> createHorizontalAndVerticalTunnels unused_lrandomints lroomrectangles

        ( newnewgrid, lLeverCoords ) =
            --fillTunnelSidesWithWalls newGrid ltunnelrectangles
            --createWallBoundary newGrid
            newGrid
                |> createWallBoundaries (ltunnelrectangles ++ lroomrectangles)
                --|> make sure if cell with x == 0 is a Floor transform to a Wall transformFloorToWallForXEqualsZero
                |> transformFloorToWallOnDisplayBoundaries
                |> correctSomeWallCorners
                |> determineCornerAndInstallLever lroomrectangles

        --|> fillTunnelSidesWithWalls ltunnelrectangles Nothing
        --|> fillTunnelSidesWithWalls lroomrectangles (Just "horizontal")
        --|> fillTunnelSidesWithWalls lroomrectangles (Just "vertical")
        ( simGrid, lLeverCoords2, lremainingrandints ) =
            GameSimulation.simulationToGetLeverPositions 20 ( newnewgrid |> GameSimulation.mbTurnNeighbourWallCellstoAshes (List.head lLeverCoords), [], lunusedrandints )

        gridAfterInstallLevers =
            newnewgrid |> installLeversInCoords lLeverCoords2

        --_ =
        --    Debug.log "list of lever coords position : " (lLeverCoords ++ lLeverCoords2)
    in
    { tileGrid = gridAfterInstallLevers, lroomRectangles = lroomrectangles, ltunnelRectangles = ltunnelrectangles, unusedRandoms = lremainingrandints }


determineCornerAndInstallLever : List GameModel.RoomRectangle -> Grid.Grid Tile -> ( Grid.Grid Tile, List Grid.Coordinate )
determineCornerAndInstallLever lrrects grid =
    let
        lcoords =
            determineLeverNearCornerRoom1Coords lrrects grid
    in
    ( installLeversInCoords lcoords grid, lcoords )


determineLeverNearCornerRoom1Coords : List GameModel.RoomRectangle -> Grid.Grid Tile -> List Grid.Coordinate
determineLeverNearCornerRoom1Coords lroomrectangles grid =
    let
        mbXYpos1 =
            lroomrectangles
                |> List.head
                |> Maybe.map (\rrect -> ( rrect.top_left_x + rrect.width - 1, rrect.top_left_y + rrect.height - 1 ))
                |> Maybe.map (\tup -> Grid.Coordinate (Tuple.first tup) (Tuple.second tup))

        mbXYpos2 =
            lroomrectangles
                |> List.head
                |> Maybe.map (\rrect -> ( rrect.top_left_x, rrect.top_left_y ))
                |> Maybe.map (\tup -> Grid.Coordinate (Tuple.first tup) (Tuple.second tup))

        lcoords =
            []
    in
    addMbElemToList mbXYpos1 lcoords


installLeversInCoords : List Grid.Coordinate -> Grid.Grid Tile -> Grid.Grid Tile
installLeversInCoords lcoords grid =
    let
        lindexAndCoords =
            List.indexedMap (\i coord -> ( i, coord )) lcoords

        setLeverFunc idx coords grid_ =
            Grid.set coords (Tile.Lever (GameModel.defaultLeverInfo idx)) grid_
    in
    List.foldl (\( idx, coords ) gridacc -> setLeverFunc idx coords gridacc) grid lindexAndCoords



--|> setLeverFunc mbXYpos2


determineRectangularRegionBoundaries : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> String -> List Grid.Coordinate
determineRectangularRegionBoundaries rrect topBotLeftRightStr =
    if topBotLeftRightStr == "top" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x rrect.top_left_y)

    else if topBotLeftRightStr == "top-1" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y - 1))

    else if topBotLeftRightStr == "top-1andCorners" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y - 1))
            |> List.append [ Grid.Coordinate (rrect.top_left_x - 1) (rrect.top_left_y - 1) ]
            |> (\lc -> List.append lc [ Grid.Coordinate (rrect.top_left_x + rrect.width) (rrect.top_left_y - 1) ])

    else if topBotLeftRightStr == "bottom" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y + rrect.height - 1))

    else if topBotLeftRightStr == "bottom+1" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y + rrect.height - 1 + 1))

    else if topBotLeftRightStr == "bottom+1andCorners" then
        List.range rrect.top_left_x (rrect.top_left_x + rrect.width - 1)
            |> List.map (\x -> Grid.Coordinate x (rrect.top_left_y + rrect.height - 1 + 1))
            |> List.append [ Grid.Coordinate (rrect.top_left_x - 1) (rrect.top_left_y + rrect.height - 1 + 1) ]
            |> (\lc -> List.append lc [ Grid.Coordinate (rrect.top_left_x + rrect.width) (rrect.top_left_y + rrect.height - 1 + 1) ])

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


determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> String -> Grid.Grid Tile -> Grid.Grid Tile
determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect topBotLeftRightStr grid =
    let
        lcoords =
            determineRectangularRegionBoundaries rrect topBotLeftRightStr
    in
    List.foldl
        (\cellcoords gridacc ->
            if Grid.get cellcoords gridacc == Just Tile.NoTileYet then
                if topBotLeftRightStr == "left-1" || topBotLeftRightStr == "left" || topBotLeftRightStr == "right" || topBotLeftRightStr == "right+1" then
                    Grid.set cellcoords (Tile.Wall GameModel.defaultWallUpInfo) gridacc

                else
                    Grid.set cellcoords (Tile.Wall GameModel.defaultWallInfo) gridacc

            else
                gridacc
        )
        grid
        lcoords


createWallBoundaries : List { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Grid.Grid Tile -> Grid.Grid Tile
createWallBoundaries lrrect grid =
    let
        fillBoundariesIfNecessary rrect grid_ =
            determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "top-1andCorners" grid_
                --determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "top-1" grid_
                |> determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "bottom+1andCorners"
                |> determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "left-1"
                |> determineRectangularRegionBoundariesAndFillWithWallIfNoTileYet rrect "right+1"
    in
    List.foldl (\rrect gridacc -> fillBoundariesIfNecessary rrect gridacc) grid lrrect


correctSomeWallCorners : Grid.Grid Tile -> Grid.Grid Tile
correctSomeWallCorners grid =
    let
        lcoords =
            Grid.toCoordinates grid

        isFourWay tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isThreeWayAtBottom tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isThreeWayAtRight tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isThreeWayAtTop tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_)

        isThreeWayAtLeft tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isTopLeftCorner tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_)

        isTopRightCorner tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_)

        isBottomRightCorner tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isBottomLeftCorner tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isInVerticalWall tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isInHorizontalWall tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_) && GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_)

        isCulDeSacAtBottom tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y - 1 } grid_)

        isCulDeSacAtTop tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x, y = coords.y + 1 } grid_)

        isCulDeSacAtLeft tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x + 1, y = coords.y } grid_)

        isCulDeSacAtRight tile coords grid_ =
            GameModel.isMbTileWall (Grid.get { x = coords.x - 1, y = coords.y } grid_)

        checkAndUpdateGriCoord coord grid_ =
            case Grid.get coord grid_ of
                Nothing ->
                    grid_

                Just tile ->
                    if isFourWay tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "four_way" tile) grid_

                    else if isThreeWayAtBottom tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "three_way_at_bottom" tile) grid_

                    else if isThreeWayAtRight tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "three_way_at_right" tile) grid_

                    else if isThreeWayAtTop tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "three_way_at_top" tile) grid_

                    else if isThreeWayAtLeft tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "three_way_at_left" tile) grid_

                    else if isTopLeftCorner tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "corner_top_left" tile) grid_

                    else if isTopRightCorner tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "corner_top_right" tile) grid_

                    else if isBottomRightCorner tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "corner_bottom_right" tile) grid_

                    else if isBottomLeftCorner tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "corner_bottom_left" tile) grid_

                    else if isInVerticalWall tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "up" tile) grid_

                    else if isInHorizontalWall tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "horizontal" tile) grid_

                    else if isCulDeSacAtBottom tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "cul_de_sac_at_bottom" tile) grid_

                    else if isCulDeSacAtTop tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "cul_de_sac_at_top" tile) grid_

                    else if isCulDeSacAtLeft tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "cul_de_sac_at_left" tile) grid_

                    else if isCulDeSacAtRight tile coord grid_ then
                        Grid.set coord (GameModel.setWallTileOrientation "cul_de_sac_at_right" tile) grid_

                    else
                        grid_
    in
    List.foldl (\coord gacc -> checkAndUpdateGriCoord coord gacc) grid lcoords


transformFloorToWallOnDisplayBoundaries : Grid.Grid Tile -> Grid.Grid Tile
transformFloorToWallOnDisplayBoundaries grid =
    let
        w =
            grid.size.width

        h =
            grid.size.height

        lcoordsLeft =
            List.map (\y_ -> { x = 0, y = y_ }) (List.range 0 (h - 1))

        lcoordsRight =
            List.map (\y_ -> { x = w - 1, y = y_ }) (List.range 0 (h - 1))

        lcoordsTop =
            List.map (\x_ -> { x = x_, y = 0 }) (List.range 0 (w - 1))

        lcoordsBottom =
            List.map (\x_ -> { x = x_, y = h - 1 }) (List.range 0 (w - 1))

        getNewGrid coord lrtd grid_ =
            --get { x, y } grid
            case Grid.get coord grid_ of
                Just (Tile.Floor _) ->
                    if lrtd == "l" || lrtd == "r" then
                        Grid.set coord (Tile.Wall GameModel.defaultWallUpInfo) grid_

                    else
                        Grid.set coord (Tile.Wall GameModel.defaultWallInfo) grid_

                _ ->
                    grid_
    in
    --if grid.get (x ,y) == Floor
    List.foldl (\coord gacc -> getNewGrid coord "l" gacc) grid lcoordsLeft
        |> (\gr -> List.foldl (\coord gacc -> getNewGrid coord "r" gacc) gr lcoordsRight)
        |> (\gr -> List.foldl (\coord gacc -> getNewGrid coord "t" gacc) gr lcoordsTop)
        |> (\gr -> List.foldl (\coord gacc -> getNewGrid coord "b" gacc) gr lcoordsBottom)


cellBelongsToMoreThanARectRegion : Grid.Coordinate -> List { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> Bool
cellBelongsToMoreThanARectRegion coord lrects =
    -- used to test if a cell belonging to a tunnel also is part of another tunnel or room
    let
        coordBelongsTorectRegion coord_ rrect =
            if coord_.x >= rrect.top_left_x && coord_.x <= rrect.top_left_x + rrect.width - 1 && coord_.y >= rrect.top_left_y && coord_.y <= rrect.top_left_y + rrect.height - 1 then
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
        coordBelongsTorectRegion coord_ rrect =
            if coord_.x >= rrect.top_left_x && coord_.x <= rrect.top_left_x + rrect.width - 1 && coord_.y >= rrect.top_left_y && coord_.y <= rrect.top_left_y + rrect.height - 1 then
                True

            else
                False
    in
    List.foldl (\roomrect bacc -> coordBelongsTorectRegion coord roomrect || bacc) False lrects


createHorizontalAndVerticalTunnels : List Int -> List GameModel.RoomRectangle -> Grid.Grid Tile -> ( Grid.Grid Tile, List GameModel.TunnelRectangle, List Int )
createHorizontalAndVerticalTunnels lrandomints lroomrectangles grid =
    let
        lrooms2 =
            List.drop 1 lroomrectangles

        ltupleRooms =
            List.map2 (\rr1 rr2 -> ( rr1, rr2 )) lroomrectangles lrooms2

        tup_ltunnels_lrandints =
            ( [], lrandomints )

        createTunnels : GameModel.RoomRectangle -> GameModel.RoomRectangle -> ( List GameModel.TunnelRectangle, List Int ) -> ( List GameModel.TunnelRectangle, List Int )
        createTunnels roomrect1 roomrect2 tupacc =
            let
                ( ltunnels, lrints ) =
                    tupacc

                newRandom =
                    List.head lrints |> Maybe.withDefault 1

                mbFilteredTunnel mbnewTunnel =
                    mbnewTunnel |> Maybe.map getRectangularRegionCellCoordinates |> Maybe.withDefault [] |> List.filter (\coords -> not (cellBelongsToARectRegion coords [ roomrect1, roomrect2 ])) |> getTunnelFromCellCoordinates

                ( mbnewTunnel1, mbnewTunnel2 ) =
                    if newRandom < 50 then
                        --createMbHorizontalAndMbVerticalTunnels roomrect1 roomrect2
                        ( mbCreateHorizontalTunnel roomrect1 roomrect2 |> mbFilteredTunnel
                        , mbCreateVerticalTunnel roomrect2 roomrect1 |> mbFilteredTunnel
                        )

                    else
                        ( mbCreateVerticalTunnel roomrect1 roomrect2 |> mbFilteredTunnel
                        , mbCreateHorizontalTunnel roomrect2 roomrect1 |> mbFilteredTunnel
                        )

                new_ltunnels =
                    addMbElemToList mbnewTunnel2 ltunnels
                        |> addMbElemToList mbnewTunnel1
            in
            ( new_ltunnels, List.drop 1 lrints )

        ( ltunnels_, lrandints ) =
            List.foldl (\( r1, r2 ) tupacc -> createTunnels r1 r2 tupacc) tup_ltunnels_lrandints ltupleRooms

        newGrid =
            listTunnelRectangleToGridFunc ltunnels_ grid
    in
    ( newGrid, ltunnels_, lrandints )


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
        horizontal_tunnel_height =
            2

        --1
        v_height =
            --just temporary , have to pass this as an argument to the function
            2

        ( start_x, tunnel_length ) =
            if GameModel.getRoomRightX roomrect1 < GameModel.getRoomCenterX roomrect2 then
                ( GameModel.getRoomRightX roomrect1 + 1, GameModel.getRoomCenterX roomrect2 - GameModel.getRoomRightX roomrect1 + (v_height - 1) )

            else if roomrect1.top_left_x > GameModel.getRoomCenterX roomrect2 then
                ( GameModel.getRoomCenterX roomrect2, roomrect1.top_left_x - GameModel.getRoomCenterX roomrect2 )

            else
                ( GameModel.getRoomCenterX roomrect1, 0 )
    in
    if tunnel_length > 0 then
        GameModel.TunnelRectangle start_x (GameModel.getRoomCenterY roomrect1) tunnel_length horizontal_tunnel_height
            --|> Debug.log "Creating Horizontal tunnel : "
            |> Just

    else
        Nothing


mbCreateVerticalTunnel : GameModel.RoomRectangle -> GameModel.RoomRectangle -> Maybe GameModel.TunnelRectangle
mbCreateVerticalTunnel roomrect1 roomrect2 =
    let
        ( start_y, tunnel_height ) =
            if GameModel.getRoomBottomY roomrect1 < GameModel.getRoomCenterY roomrect2 then
                ( GameModel.getRoomBottomY roomrect1 + 1, GameModel.getRoomCenterY roomrect2 - GameModel.getRoomBottomY roomrect1 )

            else if GameModel.getRoomTopY roomrect1 > GameModel.getRoomCenterY roomrect2 then
                ( GameModel.getRoomCenterY roomrect2, GameModel.getRoomTopY roomrect1 - GameModel.getRoomCenterY roomrect2 )

            else
                ( GameModel.getRoomCenterY roomrect1, 0 )

        vertical_tunnel_width =
            2

        --1
    in
    if tunnel_height > 0 then
        GameModel.TunnelRectangle (GameModel.getRoomCenterX roomrect1) start_y vertical_tunnel_width tunnel_height
            --|> Debug.log "Creating vertical tunnel : "
            |> Just

    else
        Nothing


listTunnelRectangleToGridFunc : List GameModel.TunnelRectangle -> Grid.Grid Tile -> Grid.Grid Tile
listTunnelRectangleToGridFunc ltunnels grid =
    -- for now , still have to write this
    listDungeonRectangleToGridFunc (List.map (\tun -> ( tun, noDoornoWallOption )) ltunnels) (Just "orange") grid


listTunnelRectangleWithOptionsToGridFunc : List ( GameModel.TunnelRectangle, GameModel.DoorWallOptions ) -> Grid.Grid Tile -> Grid.Grid Tile
listTunnelRectangleWithOptionsToGridFunc ltunnelsWithOptions grid =
    -- for now , still have to write this
    listDungeonRectangleToGridFunc ltunnelsWithOptions (Just "orange") grid


dungeonRectangleToGridFunc : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> GameModel.DoorWallOptions -> Maybe String -> Grid.Grid Tile -> Grid.Grid Tile
dungeonRectangleToGridFunc roomrect doorWallOptions mbFloorColor grid =
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

        floorColor =
            mbFloorColor |> Maybe.withDefault "default"

        defaultFloorInfoWithColor =
            GameModel.defaultFloorInfo |> (\x -> { x | color = floorColor })

        useWalls =
            False

        getDoorWallOrFloor : GameModel.DoorWallOption -> Tile
        getDoorWallOrFloor doorWallOption =
            case doorWallOption of
                GameModel.UseWall ->
                    Tile.Wall GameModel.defaultWallInfo

                GameModel.UseDoor dinfo ->
                    Tile.Door dinfo

                GameModel.NoDoorNoWall ->
                    Tile.Floor defaultFloorInfoWithColor

        generateTile : Int -> Int -> Tile
        generateTile xval yval =
            if xval == left_x && left_x /= right_x then
                getDoorWallOrFloor doorWallOptions.left

            else if xval == right_x && left_x /= right_x then
                getDoorWallOrFloor doorWallOptions.right

            else if yval == bottom_y then
                getDoorWallOrFloor doorWallOptions.bottom

            else if yval == top_y then
                getDoorWallOrFloor doorWallOptions.top

            else
                Tile.Floor defaultFloorInfoWithColor

        ltiles =
            List.concatMap (\xval -> List.map (\yval -> ( xval, yval, generateTile xval yval )) ly) lx

        new_grid =
            ltiles
                |> List.foldl (\( xval, yval, tile ) gridacc -> Grid.set (Grid.Coordinate xval yval) tile gridacc) grid
    in
    new_grid


noDoornoWallOption : GameModel.DoorWallOptions
noDoornoWallOption =
    GameModel.DoorWallOptions GameModel.NoDoorNoWall GameModel.NoDoorNoWall GameModel.NoDoorNoWall GameModel.NoDoorNoWall


listDungeonRectangleToGridFunc : List ( { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int }, GameModel.DoorWallOptions ) -> Maybe String -> Grid.Grid Tile -> Grid.Grid Tile
listDungeonRectangleToGridFunc lroomrectsWithOptions mbFloorColor grid =
    List.foldl (\( roomrect, doorWallOptions ) gridacc -> dungeonRectangleToGridFunc roomrect doorWallOptions mbFloorColor gridacc) grid lroomrectsWithOptions


listRoomRectangleToGridFunc : List GameModel.RoomRectangle -> Grid.Grid Tile -> Grid.Grid Tile
listRoomRectangleToGridFunc lroomrects grid =
    listDungeonRectangleToGridFunc (List.map (\rrect -> ( rrect, noDoornoWallOption )) lroomrects) Nothing grid



------------------------------------


getRectangularRegionCellCoordinates : { a | top_left_x : Int, top_left_y : Int, width : Int, height : Int } -> List Grid.Coordinate
getRectangularRegionCellCoordinates rectregion =
    let
        left_x =
            rectregion.top_left_x

        right_x =
            rectregion.top_left_x + rectregion.width - 1

        top_y =
            rectregion.top_left_y

        bottom_y =
            rectregion.top_left_y + rectregion.height - 1

        lx =
            List.range left_x right_x

        ly =
            List.range top_y bottom_y
    in
    List.concatMap (\xval -> List.map (\yval -> Grid.Coordinate xval yval) ly) lx


getTunnelFromCellCoordinates : List Grid.Coordinate -> Maybe GameModel.TunnelRectangle
getTunnelFromCellCoordinates lcells =
    let
        mbMinXval =
            List.map (\coords -> coords.x) lcells
                |> List.minimum

        mbMaxXval =
            List.map (\coords -> coords.x) lcells
                |> List.maximum

        mbMinYval =
            List.map (\coords -> coords.y) lcells
                |> List.minimum

        mbMaxYval =
            List.map (\coords -> coords.y) lcells
                |> List.maximum
    in
    case ( mbMinXval, mbMaxXval ) of
        ( Just minXval, Just maxXval ) ->
            case ( mbMinYval, mbMaxYval ) of
                ( Just minYval, Just maxYval ) ->
                    Just (GameModel.TunnelRectangle minXval minYval (maxXval - minXval + 1) (maxYval - minYval + 1))

                _ ->
                    Nothing

        _ ->
            Nothing
