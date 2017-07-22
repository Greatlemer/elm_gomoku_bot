module AoireClient exposing (..)

import Debug
import Dict exposing (Dict)
import Json.Decode exposing (decodeString)
import Json.Encode exposing (encode, int, object, string)
import WebSocket

type alias Server =
  { serverAddress : String
  }

type alias BoardPosition = Int
type alias TokenId = Int

type Msg
  = AwaitingFirstMove TokenId
  | TokenAllocated TokenId
  | TurnPlayed TokenId BoardPosition
  | UnknownMessage
  | WinningMove TokenId BoardPosition

connect : Server -> (String -> msg) -> Sub msg
connect server msgType =
  {-- Sub.none --}
  {--} WebSocket.listen (serverAddress server) msgType --}

initServer : String -> Server
initServer serverAddress =
  Server serverAddress

serverAddress : Server -> String
serverAddress server =
  "ws://" ++ server.serverAddress ++ "/game"

intField : String -> String -> Int
intField message key =
  case decodeString (Json.Decode.field key Json.Decode.int) message of
    Ok value ->
      value
    Err _ ->
      -1

stringField : String -> String -> String
stringField message key =
  case decodeString (Json.Decode.field key Json.Decode.string) message of
    Ok value ->
      value
    Err _ ->
      ""

wasWinningMove : String -> Bool
wasWinningMove message =
  case decodeString (Json.Decode.field "winner" Json.Decode.int) message of
    Ok _ ->
      True
    Err _ ->
      False

processMessage : String -> Msg
processMessage message =
  let
    msg = Debug.log "Received from server" message
  in
    case stringField message "type" of
      "YouAre" ->
        TokenAllocated (intField message "index")
      "Started" ->
        AwaitingFirstMove (intField message "playerIndex")
      "PlayerMove" ->
        if wasWinningMove message then
          WinningMove (intField message "playerIndex") (intField message "move")
        else
          TurnPlayed (intField message "playerIndex") (intField message "move")
      _ ->
        UnknownMessage

encodeStartGameMessage : String -> String -> String -> Int -> String
encodeStartGameMessage room botName gameType nGames =
  encode 0 (
    object
      [ ("type", Json.Encode.string "StartGame")
      , ("room", Json.Encode.string room)
      , ("userAgent", Json.Encode.string botName)
      , ("gameType", Json.Encode.string gameType)
      , ("nGames", Json.Encode.int nGames)
      ]
    )

startGame : Server -> (String -> msg) -> String -> String -> Int -> Cmd msg
startGame server msgType roomName botName numberOfGames =
  WebSocket.send (serverAddress server) (encodeStartGameMessage roomName botName "Gomoku" numberOfGames)

encodeMoveMessage : Int -> String
encodeMoveMessage move =
  encode 0 (
    object
      [ ("type", Json.Encode.string "Move")
      , ("move", Json.Encode.int move)
      ]
  )

playMove : Server -> (String -> msg) -> Int -> Cmd msg
playMove server msgType move =
  WebSocket.send (serverAddress server) (encodeMoveMessage move)