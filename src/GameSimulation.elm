module GameSimulation exposing
    ( getRandomIntBetweenValues
    , getWallTilePositionsFromGrid
    , mbTurnNeighbourWallCellstoAshes
    , randomlySelectPositionFromListAndSimulateWallToAshes
    , simulationToGetLeverPositions
    , turnNeighbourWallCellstoAshes
    )

import GameModel
import Grid
import Item exposing (Item(..), KeyInfo)
import Tile exposing (Tile(..))


mbTurnNeighbourWallCellstoAshes : Maybe Grid.Coordinate -> Grid.Grid Tile -> Grid.Grid Tile
mbTurnNeighbourWallCellstoAshes mbCoords grid =
    mbCoords
        |> Maybe.map (\coords -> turnNeighbourWallCellstoAshes coords grid)
        |> Maybe.withDefault grid


turnNeighbourWallCellstoAshes : Grid.Coordinate -> Grid.Grid Tile -> Grid.Grid Tile
turnNeighbourWallCellstoAshes { x, y } grid =
    let
        upCell =
            GameModel.location x (y - 1)

        downCell =
            GameModel.location x (y + 1)

        leftCell =
            GameModel.location (x - 1) y

        rightCell =
            GameModel.location (x + 1) y

        convertCellsFunc cellCoords grid_ =
            case Grid.get cellCoords grid_ of
                Just (Tile.Wall wallinfo) ->
                    let
                        floorinfo =
                            GameModel.defaultFloorInfo
                    in
                    Grid.set cellCoords (Tile.Floor { floorinfo | item = Just Ash }) grid_
                        |> turnNeighbourWallCellstoAshes cellCoords

                _ ->
                    grid_
    in
    convertCellsFunc upCell grid
        |> convertCellsFunc downCell
        |> convertCellsFunc leftCell
        |> convertCellsFunc rightCell


getWallTilePositionsFromGrid : Grid.Grid Tile -> List Grid.Coordinate
getWallTilePositionsFromGrid grid =
    let
        lx =
            List.range 0 grid.size.width

        ly =
            List.range 0 grid.size.height

        lgridCoords =
            List.concatMap (\xval -> List.map (\yval -> Grid.Coordinate xval yval) ly) lx

        addToListIfIsWall : Grid.Coordinate -> List Grid.Coordinate -> List Grid.Coordinate
        addToListIfIsWall coords lcoords =
            case Grid.get coords grid of
                Just (Tile.Wall wallinfo) ->
                    coords :: lcoords

                Just (Tile.WallOver woinfo) ->
                    coords :: lcoords

                _ ->
                    lcoords

        lwallcoords =
            List.foldl (\coords lcoords -> addToListIfIsWall coords lcoords) [] lgridCoords
    in
    lwallcoords


getRandomIntBetweenValues : Int -> Int -> List Int -> ( Int, List Int )
getRandomIntBetweenValues minVal maxVal lrandomInts =
    let
        randInt1To100 =
            List.head lrandomInts |> Maybe.withDefault 1

        randInt =
            minVal + round (toFloat (randInt1To100 - 1) / 100.0 * toFloat (maxVal - minVal + 1))
    in
    ( randInt, List.drop 1 lrandomInts )


randomlySelectPositionFromListAndSimulateWallToAshes : List Grid.Coordinate -> List Int -> Grid.Grid Tile -> ( Grid.Grid Tile, Maybe Grid.Coordinate, List Int )
randomlySelectPositionFromListAndSimulateWallToAshes lwallCoords lrandints grid =
    -- select wall that is in contact with at least one floor
    let
        cellHasAtLeastOneFloorNeighbour coords grid_ =
            let
                lrelevantNeighbourCells =
                    [ Grid.Coordinate (coords.x + 1) coords.y, Grid.Coordinate coords.x (coords.y + 1), Grid.Coordinate (coords.x - 1) coords.y, Grid.Coordinate coords.x (coords.y - 1) ]
            in
            List.foldl (\cell bacc -> GameModel.isFloor (Grid.get cell grid_ |> Maybe.withDefault Tile.NoTileYet) || bacc) False lrelevantNeighbourCells

        filterWalls =
            List.filter (\coords -> cellHasAtLeastOneFloorNeighbour coords grid) lwallCoords

        ( indexPos, newlrandInts ) =
            getRandomIntBetweenValues 0 (List.length filterWalls - 1) lrandints

        mbCandidateCellCoords =
            List.drop indexPos filterWalls
                |> List.head
                |> Maybe.map (\cell -> Grid.Coordinate cell.x cell.y)

        newGrid =
            case mbCandidateCellCoords of
                Just candidateCellCoords ->
                    turnNeighbourWallCellstoAshes candidateCellCoords grid

                Nothing ->
                    grid
    in
    ( newGrid, mbCandidateCellCoords, newlrandInts )


simulationToGetLeverPositions : Int -> ( Grid.Grid Tile, List Grid.Coordinate, List Int ) -> ( Grid.Grid Tile, List Grid.Coordinate, List Int )
simulationToGetLeverPositions maxNrIterations ( grid, lleverCoords, lrandints ) =
    let
        nrWallTilesThreshold =
            40

        iterateSim ( grid_, llevercoords_, lrandints_ ) =
            let
                newWallTileCoords =
                    getWallTilePositionsFromGrid grid_

                nrTiles =
                    newWallTileCoords |> List.length

                ( newgrid, mbcoords, newrandints ) =
                    randomlySelectPositionFromListAndSimulateWallToAshes newWallTileCoords lrandints_ grid_

                --_ =
                --    Debug.log "mbcoords of cell to install lever : " mbcoords
                newllevercoords =
                    case mbcoords of
                        Nothing ->
                            llevercoords_

                        Just levercoords ->
                            llevercoords_ ++ [ levercoords ]
            in
            ( newgrid, newllevercoords, newrandints )
    in
    if (getWallTilePositionsFromGrid grid |> List.length) > nrWallTilesThreshold && maxNrIterations > 0 then
        iterateSim ( grid, lleverCoords, lrandints )
            |> simulationToGetLeverPositions (maxNrIterations - 1)

    else
        ( grid, lleverCoords, lrandints )
