module MapGen exposing (..)

--import Generator
--import Generator.Standard
--import Graphics.Input as Input
--import GameView

import GameModel
import Grid


neighborhood : Grid.Coordinate -> List Grid.Coordinate
neighborhood { x, y } =
    List.map (\( a, b ) -> Grid.Coordinate a b)
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


neighborhood2 : Grid.Coordinate -> List Grid.Coordinate
neighborhood2 { x, y } =
    List.map (\( a, b ) -> Grid.Coordinate a b)
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


getNeighborsOrElse : a -> Grid.Grid a -> Grid.Coordinate -> List a
getNeighborsOrElse x grid coord =
    List.map (\c -> Grid.getWithDefault x c grid) <| neighborhood coord


getNeighborsOrElse2 : a -> Grid.Grid a -> Grid.Coordinate -> List a
getNeighborsOrElse2 x grid coord =
    List.map (\c -> Grid.getWithDefault x c grid) <| neighborhood2 coord


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



--scanl : (a -> b -> b) -> b -> List a -> List b
--Reduce a list from the left, building up all of the intermediate results into a list.
--scanl (+) 0 [1,2,3,4] == [0,1,3,6,10]


randomMap : ( Int, Int ) -> List Float -> Grid.Grid GameModel.Tile
randomMap ( w, h ) lfloats =
    let
        -- w
        -- lfloats should have w*h elements
        initialGrid =
            Grid.initialize { width = w, height = h } GameModel.NoTileYet

        -- map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]
        --set { x, y } a grid -- x < width  , y < height
        lcoordinates =
            Grid.toCoordinates initialGrid

        --List.map2 (\x y -> Grid.Coordinate x y) (List.range 0 w) (List.range 0 h)
        lcoordsFloats =
            List.map2 (\gcoord rfl -> ( gcoord, rfl )) lcoordinates lfloats
                |> Debug.log "lcoordsFloats is : "
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

        _ =
            Debug.log "just generated a random map with lfloats : "
    in
    bedrock |> iterate2 |> iterate2 |> iterate2 |> iterate2 |> iterate |> iterate |> iterate



--bedrock


seed : Int
seed =
    2013



{-
   main =
       lift display state


   display state =
       let
           level =
               fst state
       in
       flow down <|
           button
               :: map (\x -> GameView.background x `above` spacer 10 10)
                   [ level |> iterate2 |> iterate2 |> iterate2 |> iterate2 |> iterate |> iterate |> iterate ]

-}
{-


   input =
       Input.input ()


   button =
       Input.button input.handle () "clickar"


   state : Signal ( Grid.Grid GameModel.Tile, GameModel.Random )
   state =
       foldp (\a state' -> randomMap dimensions (snd state')) (randomMap dimensions gen) input.signal


   dimensions =
       ( 40, 30 )

-}
