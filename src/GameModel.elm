module GameModel exposing
    ( CurrentDisplay(..)
    , FloorStore
    , Input(..)
    , Location
    , Model
    , ModelChangerFuncs(..)
    , RoomRectangle
    , RoomType(..)
    , RoomsInfo
    , TunnelRectangle
    , getCurrentFloorInfoToStore
    , getGridTileVisibility
    , getModelTileVisibility
    , getRoomBottomY
    , getRoomCenterX
    , getRoomCenterY
    , getRoomLeftX
    , getRoomRightX
    , getRoomTopY
    , location
    , mbUpdateFightingCharacterInitiativeByMbFCharId
    , mbUpdateFightingCharacterLocation
    , placeExistingFightingCharacter
    , randomlyPlaceExistingFightingCharacters
    , setModelTileAsExplored
    , setModelTileVisibility
    , validLocation
    , visibility
    , visible
    )

import Beings.Beings as Beings
    exposing
        ( CharacterId
        , Direction(..)
        , FightingCharacter
        , FightingCharacterId
        , OPPONENT_INTERACTION_OPTIONS(..)
        , OtherCharacter
        , Player
        )
import Dict exposing (Dict)
import Grid
import Item exposing (Item(..), KeyInfo)
import Thorns.Types
import Tile exposing (Tile(..))


type CurrentDisplay
    = DisplayRegularGame
    | DisplayGameOver
    | DisplayGameCompleted
    | DisplayGameOfThorns
    | DisplayOpponentReport
    | DisplayHelpScreen
    | DisplayInventory


type ModelChangerFuncs
    = SimpleModelChanger (List (Grid.Coordinate -> Model -> Model))


type alias Model =
    { player : Player
    , fightingCharacters : Dict FightingCharacterId FightingCharacter
    , otherCharacters : Dict CharacterId OtherCharacter
    , level : Grid.Grid Tile
    , explored : Grid.Grid Tile.Visibility
    , log : List String
    , gameOfThornsModel : Thorns.Types.Model
    , listeningToKeyInput : Bool
    , pseudoRandomIntsPool : List Int
    , viewport_topleft_x : Int
    , viewport_topleft_y : Int
    , window_width : Int
    , window_height : Int
    , total_width : Int
    , total_height : Int
    , currentDisplay : CurrentDisplay
    , displayStatsOverlay : Bool
    , showBlood : Bool
    , wallPercentage : Maybe Float
    , roomsInfo : Maybe RoomsInfo
    , floorDict : Dict Int FloorStore
    , currentFloorId : Int
    , gameCompletionFunc : Int -> Grid.Coordinate -> Bool
    , leverModelChangerFuncs : Dict Tile.LeverId ModelChangerFuncs
    , started : Bool
    , debugMode : Bool
    }


type alias FloorStore =
    { level : Grid.Grid Tile
    , explored : Grid.Grid Tile.Visibility
    , window_width : Int
    , window_height : Int
    , total_width : Int
    , total_height : Int
    }


getCurrentFloorInfoToStore : Model -> FloorStore
getCurrentFloorInfoToStore model =
    { level = model.level
    , explored = model.explored
    , window_width = model.window_width
    , window_height = model.window_height
    , total_width = model.total_width
    , total_height = model.total_height
    }


type RoomType
    = SquareRoom
    | HorizontalRoom
    | VerticalRoom


type alias RoomRectangle =
    { top_left_x : Int
    , top_left_y : Int
    , width : Int
    , height : Int
    }


type alias TunnelRectangle =
    { top_left_x : Int
    , top_left_y : Int
    , width : Int
    , height : Int
    }


getRoomCenterX : RoomRectangle -> Int
getRoomCenterX roomrectangle =
    roomrectangle.top_left_x + round (toFloat roomrectangle.width / 2.0)


getRoomRightX : RoomRectangle -> Int
getRoomRightX roomrectangle =
    roomrectangle.top_left_x + roomrectangle.width - 1


getRoomLeftX : RoomRectangle -> Int
getRoomLeftX roomrectangle =
    roomrectangle.top_left_x


getRoomCenterY : RoomRectangle -> Int
getRoomCenterY roomrectangle =
    roomrectangle.top_left_y + round (toFloat roomrectangle.height / 2.0)


getRoomBottomY : RoomRectangle -> Int
getRoomBottomY roomrectangle =
    roomrectangle.top_left_y + roomrectangle.height - 1


getRoomTopY : RoomRectangle -> Int
getRoomTopY roomrectangle =
    roomrectangle.top_left_y


type alias RoomsInfo =
    { roomRectangles : List RoomRectangle
    , maxNrOfRooms : Int
    , maxRoomSize : Int
    , minRoomSize : Int
    }


type alias Location =
    Grid.Coordinate


type Input
    = Up
    | Down
    | Left
    | Right
    | PickUpItem
    | ViewInventory
    | ViewStatsOverlay
    | ViewOpponentReport
    | ViewHelpMode
    | Nop


location : Int -> Int -> Location
location =
    Grid.Coordinate


validLocation : Location -> Model -> Bool
validLocation location_ model =
    Grid.inGrid location_ model.level


isModelTileTransparent : Location -> Model -> Bool
isModelTileTransparent location_ model =
    Grid.get location_ model.level
        |> Maybe.map Tile.isTileTransparent
        |> Maybe.withDefault False


isModelTileExplored : Location -> Model -> Bool
isModelTileExplored location_ model =
    Grid.get location_ model.level
        |> Maybe.map Tile.isTileExplored
        |> Maybe.withDefault False


setModelTileAsExplored : Location -> Model -> Model
setModelTileAsExplored location_ model =
    case Grid.get location_ model.level of
        Nothing ->
            model

        Just tile ->
            let
                newTile =
                    Tile.setTileAsExplored tile
            in
            { model | level = Grid.set location_ newTile model.level }


getGridTileVisibility : Grid.Coordinate -> Grid.Grid Tile -> Tile.Visibility
getGridTileVisibility location_ gridtiles =
    Grid.get location_ gridtiles
        |> Maybe.map Tile.getTileVisibility
        |> Maybe.withDefault Tile.Unexplored


getModelTileVisibility : Location -> Model -> Tile.Visibility
getModelTileVisibility location_ model =
    Grid.get location_ model.level
        |> Maybe.map Tile.getTileVisibility
        |> Maybe.withDefault Tile.Unexplored


setModelTileVisibility : Location -> Tile.Visibility -> Model -> Model
setModelTileVisibility location_ visibility_ model =
    case Grid.get location_ model.level of
        Nothing ->
            model

        Just tile ->
            let
                newTile =
                    Tile.setTileVisibility visibility_ tile
            in
            { model | level = Grid.set location_ newTile model.level }


mbUpdateFightingCharacterInitiativeByMbFCharId : Int -> Maybe FightingCharacterId -> Model -> Model
mbUpdateFightingCharacterInitiativeByMbFCharId intval mbFightCharid model =
    case mbFightCharid of
        Nothing ->
            model

        Just fCharId ->
            let
                updatedFightingCharacters =
                    Dict.update fCharId (\mbFightChar -> mbFightChar |> Maybe.map (\fCharRec -> { fCharRec | initiative = intval })) model.fightingCharacters
            in
            { model | fightingCharacters = updatedFightingCharacters }


mbUpdateFightingCharacterLocation : Location -> Maybe FightingCharacter -> Maybe FightingCharacter
mbUpdateFightingCharacterLocation loc mbFightChar =
    case mbFightChar of
        Nothing ->
            Nothing

        Just fchar ->
            Just { fchar | location = loc, placed = True }


placeExistingFightingCharacter : FightingCharacterId -> Location -> Dict FightingCharacterId FightingCharacter -> Dict FightingCharacterId FightingCharacter
placeExistingFightingCharacter fcharid loc dictacc =
    case Dict.get fcharid dictacc of
        Nothing ->
            dictacc

        Just _ ->
            Dict.update fcharid (\mbFightChar -> mbUpdateFightingCharacterLocation loc mbFightChar) dictacc


randomlyPlaceExistingFightingCharacters : List ( Location, FightingCharacterId ) -> Model -> Model
randomlyPlaceExistingFightingCharacters lpairIntIds model =
    let
        dictFightingCharacters =
            model.fightingCharacters

        newDictFightingCharacters =
            List.foldl (\( loc, fcharid ) dictacc -> placeExistingFightingCharacter fcharid loc dictacc) dictFightingCharacters lpairIntIds
    in
    { model | fightingCharacters = newDictFightingCharacters }


visible : Model -> List Location
visible model =
    Grid.neighborhoodCalc 8 model.player.location


visibility : Model -> Location -> Tile.Visibility
visibility model location_ =
    getModelTileVisibility location_ model
