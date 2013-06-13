module DataDecl ( Player(..), Position(..), PieceType(..), PiecePosition(..), BoardState(..), Move(..), axisMin, axisMax, otherPlayer, pieceAtPosition ) where

import Data.List

data Player = White | Black deriving (Eq, Show)
data Position = Position { row :: Int, col :: Int } deriving (Eq, Show)
data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Show, Read, Bounded, Enum)
data PiecePosition = PiecePosition { pType :: PieceType, pos :: Position, color :: Player } deriving (Show)
data BoardState = BoardState { pieces :: [PiecePosition], nextPlayer :: Player } deriving (Show)
data Move = Move { from :: Position, to :: Position } deriving (Show)

axisMin = 1 :: Int
axisMax = 8 :: Int

otherPlayer :: Player -> Player
otherPlayer White = Black
otherPlayer Black = White

pieceAtPosition :: [PiecePosition] -> Position -> Maybe PiecePosition
pieceAtPosition pieces pos =
   find (\(PiecePosition _ pos' _) -> (pos == pos')) pieces
