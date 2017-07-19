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

viewBoard : Board -> Html msg
viewBoard board =
  Html.div [ boardStyle ] (Array.toList (
    Array.map viewCell board
  ))

viewCell : Cell -> Html msg
viewCell (token, turn) =
  case token of
    Token Black ->
      Html.div [blackCellStyle] [Html.text (toString turn)]
    Token Unallocated ->
      Html.div [emptyCellStyle] []
    Token White ->
      Html.div [whiteCellStyle] [Html.text (toString turn)]

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
  
blackCellStyle : Html.Attribute msg
blackCellStyle =
  style (
    [ ("backgroundColor", "black")
    , ("color", "white")
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
  
whiteCellStyle : Html.Attribute msg
whiteCellStyle =
  style (
    [ ("backgroundColor", "white")
    , ("color", "black")
    ] ++ baseCellStyleOptions
  )

-- Game