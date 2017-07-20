module Gomoku exposing (Board, Cell, isEmpty, maxPosition, newBoard, nextPlayer, placeToken, Player(..), starter, viewBoard)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Random


-- Board

type Player
  = Black
  | Unallocated
  | White

type CellContents
  = Token Player

type alias Cell
  = (CellContents, Int)
  
type alias Board =
  Array Cell

columns : Int
columns = 15

rows : Int
rows = 15

isEmpty : Board -> Int -> Bool
isEmpty board position =
  case Array.get position board of
    Just (Token Unallocated, _) ->
      True
    _ ->
      False

maxPosition : Int
maxPosition =
  (columns * rows) - 1

newBoard : Board
newBoard =
  Array.repeat (columns * rows) (Token Unallocated, 0)

nextPlayer : Player -> Player
nextPlayer currentPlayer =
  case currentPlayer of
    Black ->
      White
    Unallocated ->
      Unallocated
    White ->
      Black

-- TODO: Deal with the case where the square is non-empty to begin with
placeToken : Board -> Player -> Int -> Int -> Board
placeToken board player position turnNumber =
  Array.set position (Token player, turnNumber) board

starter : Player
starter =
  Black

-- HTML visuals

viewBoard : Board -> Int -> Html msg
viewBoard board untilTurn =
  Html.div [ boardStyle ] (Array.toList (
    Array.map (viewCell untilTurn) board
  ))

viewCell : Int -> Cell -> Html msg
viewCell untilTurn (token, turn) =
  Html.div [cellStyle turn untilTurn token] [turnText turn untilTurn]

cellStyle : Int -> Int -> CellContents -> Html.Attribute msg
cellStyle turn maxTurn token =
  if turn > maxTurn then
    emptyCellStyle
  else
    case token of
      Token Black ->
        blackCellStyle (turn == maxTurn)
      Token Unallocated ->
        emptyCellStyle
      Token White ->
        whiteCellStyle (turn == maxTurn)

turnText : Int -> Int -> Html msg
turnText turn maxTurn =
  if turn < 1 || turn > maxTurn then
    Html.text ""
  else
    Html.text (toString turn)

--   Styles
  
baseCellStyleOptions : List (String, String)
baseCellStyleOptions =
    [ ("align-items", "center")
    , ("border", "1px solid black")
    , ("display", "inline-flex")
    , ("height", "40px")
    , ("justify-content", "center")
    , ("margin", "-1px -1px 0")
    , ("vertical-align", "top")
    , ("width", "40px")
    ]
  
blackCellStyle : Bool -> Html.Attribute msg
blackCellStyle wasLastMove =
  let
    colour =
      if wasLastMove then
        "red"
      else
        "white"
  in
    style (
      [ ("backgroundColor", "black")
      , ("color", colour)
      ] ++ baseCellStyleOptions
    )

boardStyle : Html.Attribute msg
boardStyle =
  style
    [ ("line-height", "0px")
    , ("padding", "10px")
    , ("width", "600px")
    ]
  
emptyCellStyle : Html.Attribute msg
emptyCellStyle =
  style (
    [ ("backgroundColor", "#eaeaea")
    ] ++ baseCellStyleOptions
  )
  
whiteCellStyle : Bool -> Html.Attribute msg
whiteCellStyle wasLastMove =
  let
    colour =
      if wasLastMove then
        "red"
      else
        "black"
  in
    style (
      [ ("backgroundColor", "white")
      , ("color", colour)
      ] ++ baseCellStyleOptions
    )

-- Game