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
    getNeighborsOrElse GameModel.Wall


getNeighbors2 : Grid.Grid GameModel.Tile -> Grid.Coordinate -> List GameModel.Tile
getNeighbors2 =
    getNeighborsOrElse2 GameModel.Wall


numberOfWalls : Grid.Grid GameModel.Tile -> Grid.Coordinate -> Int
numberOfWalls grid coord =
    getNeighbors grid coord
        |> List.filter (\t -> t == GameModel.Wall)
        |> List.length


numberOfWalls2 : Grid.Grid GameModel.Tile -> Grid.Coordinate -> Int
numberOfWalls2 grid coord =
    getNeighbors2 grid coord
        |> List.filter (\t -> t == GameModel.Wall)
        |> List.length


randomTile : Float -> GameModel.Tile
randomTile rfloat =
    let
        tile =
            --if rfloat < 0.4 then
            if rfloat < 0.4 then
                GameModel.Wall
            else
                GameModel.Floor
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
                        GameModel.Wall
                      else
                        GameModel.Floor
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
                GameModel.Wall
            else if numberOfWalls2 grid coord <= 2 then
                GameModel.Wall
            else
                GameModel.Floor

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
    bedrock |> iterate2 |> iterate2 |> iterate2 |> iterate2 |> iterate |> iterate |> iterate



{- }

   seed : Int
   seed =
       2013
-}
