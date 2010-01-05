----------------------------------------------------------
-- Main.hs
--
-- Main module, starts game.
--
-- Author: 
-- Ramin Rakhamimov
-- http://raminrakhamimov.tk
-- ramin32@gmail.com
---------------------------------------------------------


import System.Posix
import Board

main = do
    let board = newBoard
    let turn = White
    runGame board turn

runGame board color = do
    print board
    sleep 1
    let nextBoard = executeBestMove board color 
    runGame board $ flipColor color

executeBestMove x y = x
