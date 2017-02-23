module Advent1
    ( advent1_1, advent1_2
    ) where

import Control.Monad.Writer

data Rotation = North | South | East | West deriving (Show, Eq)
data Position = Pos Int Int deriving (Show, Eq)
data State = State Position Rotation deriving (Show, Eq)
data Direction = L | R deriving (Show, Read, Eq)
data Instruction = I Direction Int deriving (Show, Read)

rotate :: Rotation -> Direction -> Rotation
rotate North L = West
rotate North R = East
rotate East L = North
rotate East R = South
rotate South L = East
rotate South R = West
rotate West L = South
rotate West R = North

step :: State -> State
step (State (Pos x y) North) = (State (Pos x     (y+1)) North)
step (State (Pos x y) East)  = (State (Pos (x+1) y)   East)
step (State (Pos x y) South) = (State (Pos x     (y-1)) South)
step (State (Pos x y) West)  = (State (Pos (x-1) y)   West)

position :: State -> Position
position (State p r) = p

walkWithSteps :: State -> Int -> [State]
walkWithSteps state blocks = take (blocks+1) $ iterate (step) state

walk :: State -> Int -> State
walk state blocks = last $ walkWithSteps state blocks

blocksAwayFromEasterBunnyHQ :: Position -> Int
blocksAwayFromEasterBunnyHQ (Pos x y) = (abs x) + (abs y)

travel :: State -> Instruction -> State
travel (State pos r) (I d n) = walk (State pos (rotate r d)) n

travelWithSteps :: State -> Instruction -> [State]
travelWithSteps (State pos r) (I d n) = walkWithSteps (State pos (rotate r d)) n

logTrip :: State -> Instruction -> Writer [Position] State
logTrip state instruction@(I _ n) = writer (travel state instruction, init $ map (position) $ travelWithSteps state instruction)

allTheSteps :: State -> [Instruction] -> Writer [Position] State
allTheSteps state instructions = do
    finalState <- foldM (logTrip) state instructions 
    tell $ [position finalState]
    return finalState

-- Parsing arguments

separate :: Char -> String -> [String]
separate c s = case dropWhile (c ==) s of
    "" -> []
    s' -> w : separate c s''
        where (w, s'') = break (c ==) s'

trim :: String -> String
trim xs = dropWhile (' ' ==) xs

toInstruction :: String -> Instruction
toInstruction (x:xs) = I (read [x]) (read xs)

-- Answers
-- 24 steps -- Blocks 14
input = "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2"
start = State (Pos 0 0) North

parsedInstructions = map (toInstruction.trim) $ separate ',' input

advent1_1 = 
    blocksAwayFromEasterBunnyHQ $ position $ foldl travel start parsedInstructions

advent1_2 = blocksAwayFromEasterBunnyHQ $ firstIntersection $ runWriter $ allTheSteps start parsedInstructions
    where firstIntersection (_,l) = firstIntersection' l []
          firstIntersection' (x:xs) l
              | elem x l = x
              | otherwise = firstIntersection' xs (x:l)
