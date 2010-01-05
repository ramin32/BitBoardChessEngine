module Board where

import Data.Word
import Data.Bits
import Data.List

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show, Enum)
data PieceSet = PieceSet {piece::Piece, board::Word64} deriving (Show)
type Player = [PieceSet]
data Board = Board {whitePlayer::Player, blackPlayer::Player} deriving (Show)

orPlayer :: Player -> Word64
orPlayer player = foldl' (\acc x -> acc .|. board x) 0 player 

orBoard :: Board -> Word64
orBoard (Board wPlayer bPlayer) = orPlayer wPlayer .|. orPlayer bPlayer


padLines n lines = n * (0x10 ^ (lines*2))

wPlayer, bPlayer :: Player
wPlayer = [  PieceSet Pawn 0xff00
           , PieceSet Rook 0x81
           , PieceSet Knight 0x42
           , PieceSet Bishop 0x24
           , PieceSet Queen 0x10
           , PieceSet King 0x08
          ]

bPlayer = [  PieceSet Pawn $ padLines 0xff 6
           , PieceSet Rook $ padLines 0x81 7
           , PieceSet Knight $ padLines 0x42 7
           , PieceSet Bishop $ padLines 0x24 7
           , PieceSet Queen $ padLines 0x10 7
           , PieceSet King $ padLines 0x08 7
          ]

newBoard = Board wPlayer bPlayer

bitsOn n 
    | n <= 0 = []  
    | otherwise = lgN: bitsOn (n - 2^lgN)
    where lgN = toInteger $ truncate $ logBase 2 n

