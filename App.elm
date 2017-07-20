module App exposing (init, Model, Msg, subscriptions, update, view)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random
import WebSocket

import AoireClient
import Gomoku
import GomokuBot exposing (Move(..), selectMove)

main : Program Flags Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { board : Gomoku.Board
  , myToken : Int
  , myColour : Gomoku.Player
  , turnNumber : Int
  , gameServer : AoireClient.Server
  }

type alias Flags =
  { serverAddress : String
  }


init : Flags -> (Model, Cmd Msg)
init {serverAddress} =
  (Model Gomoku.newBoard -1 Gomoku.Unallocated 1 (AoireClient.initServer serverAddress), Cmd.none)


-- UPDATE

type Msg
  = GenerateMove
  | PlayRandomMoveIfEmpty Int
  | ServerMessage String
  | StartGame

generateMove : Model -> (Model, Cmd Msg)
generateMove model =
  case GomokuBot.selectMove model.board model.myColour of
    Position position ->
      ( model
      , AoireClient.playMove model.gameServer (ServerMessage) position
      )
    RandomPosition ->
      (model, randomPosition model.board)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateMove ->
      generateMove model
    PlayRandomMoveIfEmpty potentialMove ->
      if Gomoku.isEmpty model.board potentialMove then
        ( model
        , AoireClient.playMove model.gameServer (ServerMessage) potentialMove
        )
      else
        (model, randomPosition model.board)
    ServerMessage msg ->
      case AoireClient.processMessage msg of
        AoireClient.AwaitingFirstMove tokenId ->
          let
            newModel = { model
                       | board = Gomoku.newBoard
                       , turnNumber = 1
                       }
          in
            if tokenId >= 0 && tokenId == model.myToken then
              generateMove newModel
            else
              (newModel, Cmd.none)
        AoireClient.TokenAllocated tokenId ->
          case tokenId of
            0 ->
              ({ model | myToken = tokenId, myColour = Gomoku.Black}, Cmd.none)
            1 ->
              ({ model | myToken = tokenId, myColour = Gomoku.White}, Cmd.none)
            _ ->
              ({ model | myToken = tokenId, myColour = Gomoku.Unallocated}
              , Cmd.none
              )
        AoireClient.TurnPlayed tokenId position ->
          if tokenId >= 0 && tokenId == model.myToken then
            (playMove model model.myColour position, Cmd.none)
          else if tokenId >= 0 then
            generateMove (playMove model (Gomoku.nextPlayer model.myColour) position)
          else
            (model, Cmd.none)
        AoireClient.WinningMove tokenId position ->
          if tokenId >= 0 && tokenId == model.myToken then
            (playMove model model.myColour position, Cmd.none)
          else if tokenId >= 0 then
            (playMove model (Gomoku.nextPlayer model.myColour) position, Cmd.none)
          else
            (model, Cmd.none)
        _ ->
          (model, Cmd.none)
    StartGame ->
      (model, AoireClient.startGame model.gameServer ServerMessage)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions {gameServer} =
  AoireClient.connect gameServer ServerMessage


-- VIEW

view : Model -> Html Msg
view {board, turnNumber} =
  div []
    [ Gomoku.viewBoard board (turnNumber - 1)
    , button [onClick StartGame] [text "Start Game"]
    , button [onClick GenerateMove] [text "Play Move"]
    ]
    
-- Bot Interface

playMove : Model -> Gomoku.Player -> Int -> Model
playMove model player position =
  { model
  | board = Gomoku.placeToken model.board player position model.turnNumber
  , turnNumber = model.turnNumber + 1
  }

randomPosition : Gomoku.Board -> Cmd Msg
randomPosition board =
  (Random.int 0 Gomoku.maxPosition)
  |> Random.generate PlayRandomMoveIfEmpty