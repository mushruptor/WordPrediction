{-# LANGUAGE BangPatterns #-}

module Main where
    
--import System.IO
import System.Environment (getArgs)
--import System.Exit
--import Data.Char
--import Text.Read
import Data.List (mapM)

--main :: IO()
main = do
    --getArgs >>= dosth >> domore >>= output
    args <- getArgs
    mapM putStrLn args
    --handle <- readFile "ngrams.arpa"
    --writeFile "test.txt" handle
    --putStrLn "Hello, please enter your word"
    --word <- getLine

