module EnumerateMoves (applyMove, enumerateMoves) where
import Data.Maybe
import DataDecl


movePiece :: Move -> PiecePosition -> Maybe PiecePosition
movePiece (Move from to) origPos@(PiecePosition pType pos color) =
   if (from == pos) then Just (PiecePosition pType to color)
   else if (to == pos) then Nothing
   else Just origPos

applyMove :: BoardState -> Move -> BoardState
applyMove (BoardState pieces player) move = 
    BoardState newPieces $ otherPlayer player 
    where newPieces = mapMaybe (movePiece move) pieces

inRange x = (x >= axisMin) && (x <= axisMax)
positionIsOnBoard (Position x y) = (inRange x) && (inRange y)

getPositionInfo :: BoardState -> Position -> String
getPositionInfo bState pos =
    if not (positionIsOnBoard pos) then "done"
    else if isNothing piece then "free"
    else if player == me then "done"
    else "captured"
    where
        BoardState pieces me = bState
        piece = pieceAtPosition pieces pos
        PiecePosition _ _ player = fromJust piece
        

-- take only positions which the piece can actually advance to
-- given:
--     board
--     last square in sequence: free, piece captured, or done
--     accumulated positions: list of legal positions
-- provide new accumulated positions
takeLegalPositions :: BoardState -> String -> [Position] -> [Position] -> [Position]
takeLegalPositions bState state accPositions remainingPositions  =
    if (remainingPositions == []) then accPositions
    else case (state) of
        "free" -> case (newState) of
                "free" -> takeLegalPositions bState "free" (newPos:accPositions) rest
                "captured" -> (newPos:accPositions)
                "done" -> accPositions
        "done" -> accPositions
    where
       (newPos:rest) = remainingPositions
       newState = getPositionInfo bState newPos

seqUp x y = [(Position (x + i) y) | i <- [1 .. axisMax]]
seqDown x y = [(Position (x - i) y) | i <- [1 .. axisMax]]
seqRight x y = [(Position x (y + i)) | i <- [1 .. axisMax]]
seqLeft x y = [(Position x (y - i)) | i <- [1 .. axisMax]]
seqUpLeft x y = [(Position (x + i) (y - i)) | i <- [1 .. axisMax]] 
seqUpRight x y = [(Position (x + i) (y + i)) | i <- [1 .. axisMax]] 
seqDownLeft x y = [(Position (x - i) (y - i)) | i <- [1 .. axisMax]] 
seqDownRight x y = [(Position (x - i) (y + i)) | i <- [1 .. axisMax]] 

positionSequencesAxial x y = [(seqUp x y), (seqDown x y), (seqRight x y), (seqLeft x y)]
positionSequencesDiagonal x y = [(seqUpLeft x y), (seqDownLeft x y), (seqUpRight x y), (seqDownRight x y)]


-----------------------------------------------------------------------------------------
-- functions which enumerate all squares which can be reached by a given piece type
-----------------------------------------------------------------------------------------
enumeratePositionsForPiece :: BoardState -> PiecePosition -> [Position]

-- enumerate Pawn's legal positions
enumeratePositionsForPiece bState (PiecePosition Pawn fromPos color) =
    takePos $ advance ++ advanceHop ++ eatLeft ++ eatRight
    where 
        Position row col = fromPos
        pawnIsAtStartPos = ((color == White) && (row == 2)) || ((color == Black) && (row == 7))
        nextRow i = row + if (color == White) then i else -i
        posAdvance i = (Position (nextRow i) col)
        posEatLeft = (Position (nextRow 1) (col - 1))
        posEatRight = (Position (nextRow 1) (col + 1))
        advanceAvailable = (getPositionInfo bState $ posAdvance 1) == "free"
        advanceHopAvailable = advanceAvailable && (getPositionInfo bState $ posAdvance 2) == "free"
        advance = if advanceAvailable then ([posAdvance 1]) else []
        advanceHop = if pawnIsAtStartPos && advanceAvailable && advanceHopAvailable
                     then [(posAdvance 2)] else []
        eatLeft = if ("captured" == getPositionInfo bState posEatLeft) then [posEatLeft] else []
        eatRight = if ("captured" == getPositionInfo bState posEatRight) then [posEatRight] else []
        takePos = (takeLegalPositions bState "free" [])

-- enumerate Rook's legal positions
enumeratePositionsForPiece bState (PiecePosition Rook fromPos _) =
    concat $ map takePos $ positionSequencesAxial row col
    where
        Position row col = fromPos 
        takePos = (takeLegalPositions bState "free" [])

-- enumerate Bishop's legal positions
enumeratePositionsForPiece bState (PiecePosition Bishop fromPos _) =
    concat $ map takePos $ positionSequencesDiagonal row col
    where
        Position row col = fromPos 
        takePos = (takeLegalPositions bState "free" [])

-- enumerate Queen's legal positions
enumeratePositionsForPiece bState (PiecePosition Queen fromPos _) =
    concat $ map takePos $ (positionSequencesAxial row col) ++ (positionSequencesDiagonal row col)
    where
        Position row col = fromPos 
        takePos = (takeLegalPositions bState "free" [])

-- enumerate Knight's legal positions
enumeratePositionsForPiece bState (PiecePosition Knight fromPos _) =
       filter posValid positions
    where
        Position row col = fromPos 
        posValid pos = ("done" /= getPositionInfo bState pos)
        positions = [(Position (row + i) (col + j)) | i <- [1,-1], j <- [2,-2]]
                 ++ [(Position (row + i) (col + j)) | i <- [2,-2], j <- [1,-1]]

-- enumerate King's legal positions
enumeratePositionsForPiece bState (PiecePosition King fromPos _) =
    filter posValid [(Position (row + i) (col + j)) | i <- [-1..1], j <- [-1..1], (i/=0) || (j/=0)]
    where
        Position row col = fromPos 
        posValid pos = ("done" /= getPositionInfo bState pos)

enumerateMovesForPiece :: BoardState -> PiecePosition -> [Move]
enumerateMovesForPiece bState p@(PiecePosition pType fromPos color) =
    map (Move fromPos) $ enumeratePositionsForPiece bState p

enumerateMoves :: BoardState -> [Move]
enumerateMoves bState@(BoardState pieces player) =
    concat $ map (enumerateMovesForPiece bState) [p | p@(PiecePosition _ _ color) <- pieces, color == player]

