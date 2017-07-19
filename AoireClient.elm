module AoireClient exposing (..)

import Debug
import Json.Encode exposing (encode, int, object, string)
import WebSocket

type alias Server =
  { serverAddress : String
  }
{--
  , room : String
  , gameType : String
  , botName : String
  , nGames : Int
  }
--}

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

processMessage : Server -> String -> Server
processMessage server message =
  let
    msg = Debug.log message
  in
    server

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

startGame : Server -> (String -> msg) -> (Server, Cmd msg)
startGame server msgType =
  ( server
  , WebSocket.send (serverAddress server) (encodeStartGameMessage "Elm Bots ltd." "ElmBot v0.1 (by Adrian)" "gomoku" 1)
  )

--      (Model (placeToken nextSquare board) (nextSquare + 1) token, WebSocket.send gameServer (encodeStartGameMessage "Elm Bots ltd." "ElmBot v0.1 (by Adrian)" "Gomoku" 1))