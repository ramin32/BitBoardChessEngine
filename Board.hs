module Board where

import Data.Word
import Data.Bits
import Data.List

data PieceSet = PieceSet {setType::Piece, board::Word64} deriving (Show)
data Player = Player {playerColor::Color, pieces::[PieceSet]}
data Board = Board [Player]
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show, Enum)
data Color = White | Black deriving (Eq, Show)
data ColoredPiece = ColoredPiece {pieceColor::Color, piece::Piece} deriving (Eq, Show)

instance Show Board where
    show board = ""

pieceAt :: Word64 -> Board -> Maybe ColoredPiece
pieceAt i (Board board) 
    | not $ null matches = head matches
    | otherwise = Nothing
    where matches = map (playerPieceAt i) board

playerPieceAt :: Word64 -> Player -> Maybe ColoredPiece
playerPieceAt i (Player color pieces) 
    | not $ null matches = Just (ColoredPiece color $ setType $ head matches)
    | otherwise = Nothing
    where matches = filter (\x -> i .|. (board x) > 0) pieces

flipColor White = Black
flipColor Black = White

padLines :: Word64 -> Int -> Word64
padLines n lines = n * (0x10 ^ (lines*2))

whitePlayer, blackPlayer :: Player
whitePlayer = Player White [  PieceSet Pawn 0xff00
           , PieceSet Rook 0x81
           , PieceSet Knight 0x42
           , PieceSet Bishop 0x24
           , PieceSet Queen 0x10
           , PieceSet King 0x08
          ]

blackPlayer = Player Black 
          [  PieceSet Pawn $ padLines 0xff 6
           , PieceSet Rook $ padLines 0x81 7
           , PieceSet Knight $ padLines 0x42 7
           , PieceSet Bishop $ padLines 0x24 7
           , PieceSet Queen $ padLines 0x10 7
           , PieceSet King $ padLines 0x08 7
          ]

newBoard :: Board
newBoard = Board [whitePlayer, blackPlayer]

bitsOn :: (Floating a, RealFrac a) => a -> [Integer]
bitsOn n 
    | n <= 0 = []  
    | otherwise = lgN: bitsOn (n - 2^lgN)
    where lgN = toInteger $ truncate $ logBase 2 n

