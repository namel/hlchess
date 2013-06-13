module EvalBoard (accumulateBestScoreBoard, calcMoveScore, chooseBestSubMove) where
import DataDecl
import EnumerateMoves
import Data.Maybe

pieceVal :: PieceType -> Float
pieceVal Pawn = 1
pieceVal Knight = 3
pieceVal Bishop = 3
pieceVal Rook = 5
pieceVal Queen = 9
pieceVal King = 100

locationScores = [0.0, 0.0, 0.0, 0.02, 0.04, 0.04, 0.02, 0.0, 0.0]
positionVal (Position row col) = (locationScores !! row) + (locationScores !! col)

playerPieceValue :: PiecePosition -> Float
playerPieceValue (PiecePosition pType pos p ) = 
    if (p == Black) then (-val) else val
    where 
        val = pieceVal pType + positionVal pos
        

-- evaluate the board score.  The higher the score, the better White is doing.
evalBoard :: BoardState -> Float
evalBoard (BoardState pieces _) =
    foldl (\acc piece -> acc + (playerPieceValue piece)) 0 pieces

-- recursively evaluate the score of a move, looking at submoves if necessary
calcMoveScore :: BoardState -> Int -> Move -> Float
calcMoveScore bState depth move =
    if (depth == 0) 
        then (evalBoard nextBoard) 
    else 
        snd (chooseBestSubMove nextBoard depth)
    where
        nextBoard = applyMove bState move

-- consider a possible move and keep it if it's the best so far.
accumulateBestScoreBoard :: Int -> BoardState -> Maybe (Move, Float) -> Move -> Maybe (Move, Float)
accumulateBestScoreBoard depth bState bestOptionYet move 
    | isNothing bestOptionYet = Just (move, score)
    | (score `betterThan` bestScoreYet) = Just (move, score)
    | otherwise = bestOptionYet
    where
        bestScoreYet = snd $ fromJust bestOptionYet
        BoardState _ nextPlayer = bState
        betterThan = if (nextPlayer == White) then ( > ) else ( < )
        score = calcMoveScore bState depth move

-- given a board, consider all submoves and return the best move and its score.
chooseBestSubMove :: BoardState -> Int -> (Move, Float)
chooseBestSubMove bState depth =
    fromJust $ foldl acc Nothing subMoves
    where
       BoardState _ lastPlayer = bState
       subMoves = enumerateMoves bState
       acc = accumulateBestScoreBoard (depth - 1) bState

    
