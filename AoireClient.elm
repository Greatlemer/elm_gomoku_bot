module AoireClient exposing (..)

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
  server

{--
encodeStartGameMessage : Server -> String
encodeStartGameMessage {room, gameType, botName, nGames} =
  encode 0 (
    object
      [ ("type", Json.Encode.string "StartGame")
      , ("room", Json.Encode.string room)
      , ("userAgent", Json.Encode.string botName)
      , ("gameType", Json.Encode.string gameType)
      , ("nGames", Json.Encode.int nGames)
      ]
    )
--}

--      (Model (placeToken nextSquare board) (nextSquare + 1) token, WebSocket.send gameServer (encodeInitialMessage "Elm Bots ltd." "ElmBot v0.1 (by Adrian)" "gomoku" 1))