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
  , currentPlayer : Gomoku.Player
  , gameServer : AoireClient.Server
  }

type alias Flags =
  { serverAddress : String
  }


init : Flags -> (Model, Cmd Msg)
init {serverAddress} =
  (Model Gomoku.newBoard Gomoku.starter (AoireClient.initServer serverAddress), Cmd.none)


-- UPDATE

type Msg
  = GenerateMove
  | PlayRandomMoveIfEmpty Int
  | ServerMessage String
  | StartGame

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GenerateMove ->
      case GomokuBot.selectMove model.board model.currentPlayer of
        Position position ->
          (playMove model position, Cmd.none)
        RandomPosition ->
          (model, randomPosition model.board)
    PlayRandomMoveIfEmpty potentialMove ->
      if Gomoku.isEmpty model.board potentialMove then
        (playMove model potentialMove, Cmd.none)
      else
        (model, randomPosition model.board)
    ServerMessage msg ->
      ( { model | gameServer = AoireClient.processMessage model.gameServer msg }
      , Cmd.none
      )
    StartGame ->
      let
        (server, cmd) = AoireClient.startGame model.gameServer ServerMessage
      in
        ({ model | gameServer = server }, cmd)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions {gameServer} =
  AoireClient.connect gameServer ServerMessage


-- VIEW

view : Model -> Html Msg
view {board} =
  div []
    [ Gomoku.viewBoard board
    , button [onClick StartGame] [text "Start Game"]
    , button [onClick GenerateMove] [text "Play Move"]
    ]
    
-- Bot Interface

playMove : Model -> Int -> Model
playMove model position =
  let
    nextPlayer = Gomoku.nextPlayer model.currentPlayer
    nextBoard = Gomoku.placeToken model.board model.currentPlayer position
  in
    { model |
        board = nextBoard,
        currentPlayer = nextPlayer
    }

randomPosition : Gomoku.Board -> Cmd Msg
randomPosition board =
  (Random.int 0 Gomoku.maxPosition)
  |> Random.generate PlayRandomMoveIfEmpty