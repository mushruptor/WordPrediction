{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment (getArgs)
import Data.List 
import Text.Parsec
import Control.Monad.State.Lazy hiding (mapM)
import Data.Char
import Data.Function

data Ngram = Ngram { weight :: Float, ngram :: [String], fallback :: Float}     deriving (Show, Read)

testList = [Ngram (-1.0) ["abs", "cbd"] (-0.5),
            Ngram (-1.7) ["titr", "ijr"] (-0.2),
            Ngram (-1.9) ["titr", "isd"] (-0.2),
            Ngram (-2.0) ["titr", "idfr", "jaw"] (-0.2),
            Ngram (-2.1) ["titr", "idfr", "akksk"] (-0.2)]

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

parseString :: String -> [String]
parseString = filter (not . null) . (\x -> map trim x) . (groupBy ((==) `on` isAlpha))
    where
        trim :: String -> String
        trim = f . f
        f = reverse . dropWhile isSpace

-- | return top k results sorted descending by ngram weigths
topK :: Int -> [Ngram] -> [Ngram]
topK k = take k . sortOn weight

-- | takes a list of ngrams and a list of strings of consecutive occuring words
-- and returns all ngrams matching those words
-- Note that the last word of each ngram is cut in order to predict the next word
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

fromARPAFile :: FilePath -> IO ()
fromARPAFile fp = do
    words <- readFile fp
    putStrLn "Hello"
