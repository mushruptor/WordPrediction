{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment (getArgs)
import Data.List 
import Text.Parsec
import Control.Monad.State.Lazy hiding (mapM)
import Data.Maybe

data Ngram = Ngram { weight :: Float, ngram :: [String], fallback :: Float}     deriving (Show, Read)
type Dictionary = (Int, [Ngram])

testList = [Ngram (-1.0) ["abs", "cbd"] (-0.5),
            Ngram (-1.5) ["titr", "ijr"] (-0.2)]

main = do
    --getArgs >>= dosth >> domore >>= output
    args <- getArgs
    mapM putStrLn args
    fromARPAFile "test.txt"
    --handle <- readFile "ngrams.arpa"
    --writeFile "test.txt" handle
    --putStrLn "Hello, please enter your word"
    --word <- getLine

--parseLine = do
  --first <- count 4 anyChar
  --second <- count 4 anyChar
  --return (first, second)

--parseFile = endBy parseLine (char '\n')

-- | returns all Ngrams matching a list of words
compareNgram :: [Ngram] -> [String] -> [Ngram]
compareNgram ns cs = matching (filter (\n -> length (ngram n) - 1 == length cs) ns) cs 
    where
        matching :: [Ngram] -> [String] -> [Ngram]
        matching [] _ = []
        matching _ [] = [] 
        matching (n:ns) cs 
          | areEqualNS (reduce n) cs = n : matching ns cs
          | otherwise = matching ns cs        
        
        reduce :: Ngram -> Ngram
        reduce (Ngram a xs b) = Ngram a (take (length xs - 1) xs) b

        areEqualNS :: Ngram -> [String] -> Bool
        areEqualNS (Ngram _ s1 _) s2 = s1 == s2

topK :: Int -> [Ngram] -> [Ngram]
topK k = take k . sortOn weight

fromARPAFile :: FilePath -> IO ()
fromARPAFile fp = do
    words <- readFile fp
    putStrLn "Hello"
