module Move where

file i = i `mod` 8
rank i = i `div` 8

posToWord file rank = 2 ^ (rank * 8 + file) 

distance i j = (abs $ rank i - rank j,
                abs $ file i - file j)

-- general conditions
sameRank, diagnalMove, knightMove :: Int -> Int -> Bool
sameRank i j = rank i == rank j
sameFile i j = file i == file j
diagnalMove i j = let d = distance i j
                in fst d == snd d
onBoard j = 0 <= j && j < 64

-- piece conditions
rookMove i j = sameRank i j || sameFile i j && onBoard j
knightMove i j = let d = distance i j
                 in elem d [(2,1),(1,2)] && onBoard j
bishopMove i j = diagnalMove i j && onBoard j
kingMove i j = let d = distance i j
               in elem d [(0,1), (1,0), (1,1)] && onBoard j

-- Moves
horizontalMoves, verticalMoves, diagnalMoves, knightMoves :: [Int -> Int] 
horizontalMoves = [(+1), (subtract 1)]
verticalMoves = [(+8), (subtract 8)]
diagnalMoves = [(+7), (+9), (subtract 7), (subtract 9)]
knightMoves = [(+6),(+10),(+15),(+17), (subtract 6),(subtract 10),(subtract 15),(subtract 17)]

allConditions :: [a -> Bool] -> a -> Bool
allConditions conditions i = all ($i) conditions

generateMoves moves conditions i = 
    concatMap 
        ((takeWhile $ allConditions conditions).tail.(`iterate` i)) 
        moves 

-- Move Iterators
iterateRook i = generateMoves horizontalMoves [onBoard, sameRank i] i ++
                generateMoves verticalMoves [onBoard] i
iterateKnight i = generateMoves knightMoves [onBoard, knightMove i] i 
iterateBishop i = generateMoves diagnalMoves [onBoard, diagnalMove i] i 
iterateQueen i = iterateBishop i ++ iterateRook i
iterateKing i = filter (allConditions [onBoard, kingMove i]) $ 
                map ($i) (diagnalMoves ++ horizontalMoves ++ verticalMoves)

iterateWhitePawn i 
    | rank i == 1 = [i + 8, i + 16]
    | otherwise = filter onBoard [i + 8]

iterateBlackPawn i 
    | rank i == 6 = [i - 8, i - 16]
    | otherwise = filter onBoard [i - 8]

