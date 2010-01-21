module Piece where

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Enum)
data Color = White | Black deriving (Eq)
data ColoredPiece = ColoredPiece Piece Color deriving (Eq) 

instance Show Piece where
    show piece = case piece of
        Pawn -> "P"
        Knight -> "N"
        Bishop -> "B"
        Rook -> "R"
        Queen -> "Q"
        King -> "K"

instance Show Color where
    show White = "W"
    show Black = "B"

showMaybeColoredPiece :: Maybe ColoredPiece -> String
showMaybeColoredPiece (Just (ColoredPiece piece color)) = show piece ++ show color
showMaybeColoredPiece Nothing = "  " 
