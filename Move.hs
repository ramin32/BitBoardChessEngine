module Move where

import Data.Word
import Piece
import Bit
import Data.Bits

type Move = Int
type Position = Word64
type MoveCondition = Position -> Position -> Bool
data Moves = Moves {moves::[Move], condition::[MoveCondition]} 


file i = mod i (bit 8)
rank i = div i (bit 8)

posToWord file rank = bit (rank * 8 + file) 

distance i j = (abs $ rank i - rank j,
                abs $ file i - file j)

-- general conditions
sameRank, sameFile, diagnalMove :: MoveCondition
sameRank i j = rank i == rank j
sameFile i j = file i == file j
diagnalMove i j = let d = distance i j
                in fst d == snd d

onBoard :: Position -> Bool
onBoard j = (bit 0) <= j && j <= (bit 63)

-- piece conditions
rookMove, knightMove, bishopMove, kingMove :: MoveCondition
rookMove i j = sameRank i j || sameFile i j 
knightMove i j = let d = distance i j
                 in elem d [(2,1),(1,2)] 
bishopMove i j = diagnalMove i j 
kingMove i j = let d = distance i j
               in elem d [(0,1), (1,0), (1,1)] 

concatMoves :: [Moves] -> Moves
concatMoves movesList = foldl1 
                        (\(Moves ms cs) (Moves ms1 cs1) -> Moves (ms++ms1) (cs ++ cs1)) 
                        movesList
-- Moves
positiveHorizontalMoves, negativeHorizontalMoves, horizontalMoves, verticalMoves, positiveDiagnalMoves, negativeDiagnalMoves, diagnalMoves, knightMoves, allDirectionMoves :: Moves

positiveHorizontalMoves = Moves [1] [sameRank]
negativeHorizontalMoves = Moves [-1] [sameRank]
horizontalMoves = concatMoves [positiveHorizontalMoves, negativeHorizontalMoves]
verticalMoves = Moves [8, -8] [sameFile]
positiveDiagnalMoves = Moves [7, 9] [diagnalMove] 
negativeDiagnalMoves = Moves [-7, -9] [diagnalMove]
diagnalMoves = concatMoves [positiveDiagnalMoves, negativeDiagnalMoves] 
knightMoves = Moves [6,10,15,17, -6,-10,-15,-17] [knightMove]
allDirectionMoves = concatMoves [horizontalMoves, verticalMoves, diagnalMoves]

allConditions :: [MoveCondition] -> Position -> Position -> Bool
allConditions conditions p1 p2 = (onBoard p2) && (and $ map (\c -> c p1 p2) conditions)
 
generateMoves :: Moves -> Position -> [Position]
generateMoves (Moves moves conditions) pos = 
    concatMap (\m -> takeWhile (allConditions conditions pos) $ 
                     tail $ 
                     iterate (`shift` m) pos
              ) 
              moves  

generateSingleMoves :: Moves -> Position -> [Position]
generateSingleMoves (Moves moves conditions) pos = 
    takeWhile (allConditions conditions pos) $ map (shift pos) moves

iterateMoves :: ColoredPiece -> Position -> [Position]
iterateMoves coloredPiece from = case coloredPiece of
    (ColoredPiece Rook _) -> generateMoves horizontalMoves from ++
                             generateMoves verticalMoves from
    (ColoredPiece Knight _) -> generateMoves knightMoves from 
    (ColoredPiece Bishop _) -> generateMoves diagnalMoves from 
    (ColoredPiece Queen c)  -> iterateMoves (ColoredPiece Bishop c) from ++ 
                               iterateMoves (ColoredPiece Rook c) from
    (ColoredPiece King _)  -> generateSingleMoves allDirectionMoves from
    (ColoredPiece Pawn White)  
        | rank from == 1 -> [from + 8, from + 16]
        | otherwise -> filter onBoard [from + 8]
    (ColoredPiece Pawn Black) 
        | rank from == 6 -> [from - 8, from - 16]
        | otherwise -> filter onBoard [from - 8]

