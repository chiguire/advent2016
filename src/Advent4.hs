module Advent4
    ( advent4_1, advent4_2
    ) where

data Room = Room String Int String
name (Room n _ _) = n
sectorId (Room _ id _) = id
checksum (Room _ _ cs) = cs

--deriveChecksum :: String -> String
--deriveChecksum s = map (\x -> (x, 1)) s
-- encode
-- sort
-- take 5

-- Answers

advent4_1 = ""

advent4_2 = ""

-- Input
