module GomokuBot exposing (Move(..), selectMove)

import Gomoku

type Move
  = Position Int
  | RandomPosition

type Strategy
  = BlockForcedWin
  | BlockWinner
  | PlayRandom
  | PlayWinner

-- Bot Logic

initialStrategy : Strategy
initialStrategy = PlayWinner

nextStrategy : Strategy -> Strategy
nextStrategy currentStrategy =
  case currentStrategy of
    PlayWinner ->
      BlockWinner
    BlockWinner ->
      BlockForcedWin
    BlockForcedWin ->
      PlayRandom
    PlayRandom ->
      PlayRandom

selectMove : Gomoku.Board -> Gomoku.Player -> Move
selectMove board player =
  runThroughStrategies board player initialStrategy

runThroughStrategies : Gomoku.Board -> Gomoku.Player -> Strategy -> Move
runThroughStrategies board player currentStrategy =
  case runStrategy board player currentStrategy of
    Just move ->
      move
    Nothing ->
      runThroughStrategies board player (nextStrategy currentStrategy)

runStrategy : Gomoku.Board -> Gomoku.Player -> Strategy -> Maybe Move
runStrategy board player strategy =
  case strategy of
    PlayWinner ->
      findWinningMove board player
    BlockWinner ->
      findWinningMove board (Gomoku.nextPlayer player)
    BlockForcedWin ->
      findWinForcer board (Gomoku.nextPlayer player)
    PlayRandom ->
      Just RandomPosition

findWinningMove : Gomoku.Board -> Gomoku.Player -> Maybe Move
findWinningMove board player =
  Nothing

findWinForcer : Gomoku.Board -> Gomoku.Player -> Maybe Move
findWinForcer board player =
  Nothing