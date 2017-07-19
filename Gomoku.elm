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

type Cell
  = Token Player
  
type alias Board =
  Array Cell

columns : Int
columns = 15

rows : Int
rows = 15

isEmpty : Board -> Int -> Bool
isEmpty board position =
  Array.get position board == Just (Token Unallocated)

maxPosition : Int
maxPosition =
  (columns * rows) - 1

newBoard : Board
newBoard =
  Array.repeat (columns * rows) (Token Unallocated)

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
placeToken : Board -> Player -> Int -> Board
placeToken board player position =
  Array.set position (Token player) board

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
viewCell cell =
  case cell of
    Token Black ->
      Html.div [blackCellStyle] []
    Token Unallocated ->
      Html.div [emptyCellStyle] []
    Token White ->
      Html.div [whiteCellStyle] []

--   Styles
  
baseCellStyleOptions : List (String, String)
baseCellStyleOptions =
    [ ("border", "1px solid black")
    , ("display", "inline-block")
    , ("height", "40px")
    , ("margin", "-1px -1px 0")
    , ("width", "40px")
    ]
  
blackCellStyle : Html.Attribute msg
blackCellStyle =
  style (
    [ ("backgroundColor", "black")
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
    ] ++ baseCellStyleOptions
  )

-- Game