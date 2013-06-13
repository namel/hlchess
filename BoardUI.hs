module BoardUI (boardReader, showBoard, moveReader) where

import DataDecl
import Char
import Data.Maybe


pieceReader :: (Char, (Int, Int)) -> Maybe PiecePosition
pieceReader (c, (row, col))
  | c  == ' ' = Nothing
  | c' == 'p' = makePiece Pawn
  | c' == 'r' = makePiece Rook
  | c' == 'n' = makePiece Knight
  | c' == 'b' = makePiece Bishop
  | c' == 'q' = makePiece Queen 
  | c' == 'k' = makePiece King
  where color = if (isLower c) then White else Black
        c' = toLower c
        pos = Position row col
        makePiece a = Just (PiecePosition a pos color)


boardReader :: [String] -> Player -> BoardState
boardReader rowDesc nextPlayer =
    BoardState (mapMaybe pieceReader allPiecesPositions) nextPlayer
    where 
        allPieces = concat rowDesc
        allPiecesPositions = zip allPieces [ (row,col) | row <- [8,7..1], col <- [1..8]]


pieceSymbol :: [PiecePosition] -> (Int, Int) -> Char
pieceSymbol pieces (row, col) 
    | col ==  9 = '\n'
    | row ==  0 = ' '
    | col ==  0 = ' '
    | col == -1 = head $ show row
    | row == -1 = chr $ col + ord 'a' - 1
    | isNothing piece = ' '
    | pType == Pawn = adjustColor 'p'
    | pType == Rook = adjustColor 'r'
    | pType == Knight = adjustColor 'n'
    | pType == Bishop = adjustColor 'b'
    | pType == Queen = adjustColor 'q'
    | pType == King = adjustColor 'k'
    where 
        piece = pieceAtPosition pieces (Position row col)
        PiecePosition pType _ player = fromJust piece
        adjustColor c = if player == White then c else toUpper c


showBoard :: BoardState -> IO ()
showBoard (BoardState pieces player) = do
    putStrLn boardString
    where symbolMapper = (pieceSymbol pieces)
          boardString = map symbolMapper [ (row,col) | row <- [8,7..(-1)], col <- [(-1),0..9]] 


coordReader :: [Char] -> Position
coordReader coord = Position row col
    where
       (x:y) = coord
       col = ord (x) - ord ('a') + 1
       row = read y :: Int


moveReader :: String -> Move
moveReader s = Move fromPos toPos
    where
       [from, to] = words s
       fromPos = coordReader from
       toPos = coordReader to

