import DataDecl 
import EvalBoard
import EnumerateMoves
import BoardUI
import Char 
import Data.Maybe

defaultDepth = 5

piecesStart = ["RNBQKBNR",
               "PPPPPPPP",
               "        ",
               "        ",
               "        ",
               "        ",
               "pppppppp",
               "rnbqkbnr"]

boardStart = boardReader piecesStart White

-----------------------------------------------------------------------------------------
-- this module provides two services:
--     showScores: given a board, it will show the score for every legal move
--     playGame: starts a new game and interacts with the user
-----------------------------------------------------------------------------------------
showScores bState = 
    zip subMoves (map calcScore subMoves)
    where 
        subMoves = enumerateMoves bState
        calcScore = (calcMoveScore bState defaultDepth)


moveInteract bState = do
    showBoard $ bState
    putStrLn "Your move"
    humanMove <- getLine
    if (humanMove == "q") 
        then return ()
        else do
            let 
                bStateAfterHumanMove = applyMove bState $ moveReader humanMove
                bestMove = fst $ chooseBestSubMove bStateAfterHumanMove defaultDepth
                bStateAfterComputerReply = applyMove bStateAfterHumanMove bestMove
            moveInteract bStateAfterComputerReply


playGame = do 
        putStrLn "Welcome to the simple haskell chess program."
        moveInteract boardStart

main = playGame
-- TODO: use interact to implement a cleaner version of main.
-- just do main = interact moveInteraction
-- where move interaction is a function which receives a 'q' or a legal move, and returns a string which
-- shows the board state.
