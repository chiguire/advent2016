module Lib
    ( execLib
    ) where

import Advent1
import Advent2
import Advent3

execLib :: String -> String
execLib x = case x of "advent1_1" -> show advent1_1
                      "advent1_2" -> show advent1_2
                      "advent2_1" -> show advent2_1
                      "advent2_2" -> show advent2_2
                      "advent3_1" -> show advent3_1
                      "advent3_2" -> show advent3_2
                      otherwise -> "Failed!"
