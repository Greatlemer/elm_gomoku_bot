module App exposing (init, Model, Msg, subscriptions, update, view)

import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
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
  , gameRoom : String
  , gameAuthor : String
  , gameBotName : String
  , gameSeriesLength : Int
  }

type alias Flags =
  { serverAddress : String
  }


init : Flags -> (Model, Cmd Msg)
init {serverAddress} =
  (Model Gomoku.newBoard -1 Gomoku.Unallocated 1 (AoireClient.initServer serverAddress) defaultRoom defaultBotAuthor defaultBotName defaultNumberOfGames, Cmd.none)


-- UPDATE

type Msg
  = InputBotAuthor String
  | InputBotName String
  | InputNumberOfGames String
  | InputRoom String
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
      let
        botName = model.gameBotName ++ " (by " ++ model.gameAuthor ++ ")"
      in
        ( model
        , AoireClient.startGame model.gameServer ServerMessage model.gameRoom botName model.gameSeriesLength
        )
    InputBotAuthor author ->
      ({ model | gameAuthor = author }, Cmd.none)
    InputBotName botName ->
      ({ model | gameBotName = botName }, Cmd.none)
    InputNumberOfGames seriesLength ->
      ( { model | gameSeriesLength = Result.withDefault model.gameSeriesLength (String.toInt seriesLength) }
      , Cmd.none
      )
    InputRoom room ->
      ({ model | gameRoom = room }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions {gameServer} =
  AoireClient.connect gameServer ServerMessage

-- DEFAULTS

defaultBotAuthor : String
defaultBotAuthor =
  "Adrian"

defaultBotName : String
defaultBotName =
  "Elmoku v0.1"

defaultNumberOfGames : Int
defaultNumberOfGames =
  2

defaultRoom : String
defaultRoom =
  "Elm Bots ltd."

-- STYLES

styleBoardAndControlContainer : Html.Attribute Msg
styleBoardAndControlContainer =
  Html.Attributes.style
    [ ("display", "flex")
    ]

styleSidebar : Html.Attribute Msg
styleSidebar =
  Html.Attributes.style []

styleSidebarInput : Html.Attribute Msg
styleSidebarInput =
  Html.Attributes.style
    [ ("display", "inline-block")
    , ("padding-left", "1%")
    , ("padding-right", "1%")
    , ("width", "46%")
    ]

styleSidebarLabel : Html.Attribute Msg
styleSidebarLabel =
  Html.Attributes.style
    [ ("display", "inline-block")
    , ("width", "50%")
    ]

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ viewHeading
    , viewBoardAndControls model
    ]

viewBoardAndControls : Model -> Html Msg
viewBoardAndControls ({board, turnNumber} as model) =
  div [styleBoardAndControlContainer]
    [ Gomoku.viewBoard board (turnNumber - 1)
    , viewControlSidebar model
    ]

viewButton : String -> Msg -> Html Msg
viewButton buttonText clickEvent =
  button [onClick clickEvent] [text buttonText]

viewControlSidebar : Model -> Html Msg
viewControlSidebar model =
  div [styleSidebar]
    [ viewTextInput "Game Room: " defaultRoom InputRoom
    , viewTextInput "Bot Author: " defaultBotAuthor InputBotAuthor
    , viewTextInput "Bot Name: " defaultBotName InputBotName
    , viewNumberInput "Number of Games: " defaultNumberOfGames InputNumberOfGames
    , viewButton "Start Game Series" StartGame
    ]

viewHeading : Html Msg
viewHeading =
  Html.h1 [] [Html.text "Elm bots can play Gomoku too!"]

viewNumberInput : String -> Int -> (String -> Msg) -> Html Msg
viewNumberInput labelText default inputType =
  div []
    [ Html.label [styleSidebarLabel] [Html.text labelText]
    , Html.input
      [ Html.Attributes.type_ "number"
      , Html.Events.onInput inputType
      , Html.Attributes.defaultValue (toString default)
      , styleSidebarInput
      ] []
    ]

viewTextInput : String -> String -> (String -> Msg) -> Html Msg
viewTextInput labelText default inputType =
  div []
    [ Html.label [styleSidebarLabel] [Html.text labelText]
    , Html.input
      [ Html.Attributes.type_ "text"
      , Html.Events.onInput inputType
      , Html.Attributes.defaultValue default
      , styleSidebarInput
      ] []
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