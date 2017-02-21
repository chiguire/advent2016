module Main where

import Lib
import System.Environment
import Control.Monad.Writer

execArgs :: [String] -> String
execArgs [] = "You must specify at least one argument: <executable> <parameter>"
execArgs (x:[]) = execArgs' x

execArgs' :: String -> String
execArgs' x = execLib x

main :: IO ()
main = do
    args <- getArgs
    putStrLn (execArgs args)
