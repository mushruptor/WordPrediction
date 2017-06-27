{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment (getArgs)
import Data.List 
import Control.Monad.State.Lazy hiding (mapM)
import Data.Char
import Data.Function
import Data.Typeable

data Ngram = Ngram { weight :: Float, ngram :: [String], fallback :: Float}     deriving (Show, Read)
data PosString = PosString Int Int String                                       deriving (Show, Read)

testList = [Ngram (-1.0) ["abs", "cbd"] (-0.5),
            Ngram (-1.7) ["titr", "ijr"] (-0.2),
            Ngram (-1.9) ["titr", "isd"] (-0.2),
            Ngram (-2.0) ["titr", "idfr", "jaw"] (-0.2),
            Ngram (-2.1) ["titr", "idfr", "akksk"] (-0.2)]

main :: IO ()
main = do
    args <- getArgs
    tralala <- parseARPAFile (args !! 2)
    printNgrams tralala
    putStrLn "Done"

-- | print ngram
printNgram :: Ngram -> IO ()
printNgram (Ngram a ns b) = putStrLn $ show a ++ "  " ++ unwords ns ++ "  "  ++ show b

-- | print list of ngrams
printNgrams :: [Ngram] -> IO [()] 
printNgrams = mapM printNgram

-- | print the first k ngrams
printkNgrams :: Int -> [Ngram] -> IO [()]
printkNgrams k ns = mapM printNgram (take k ns)
    
-- | generate a ngram from a string
parseNgram :: String -> Ngram
parseNgram s = go 5 (words s)
    where
        -- | if n = k there is no fallback weight -> set it to 0
        go :: Int -> [String] -> Ngram
        go k (x:xs) 
          | length xs >= k = Ngram (read x) xs 0.0 
          | otherwise = Ngram (read x) (init xs) (read (last xs))

-- | parse the .arpa file to a list of ngrams
parseARPAFile :: String -> IO [Ngram]
parseARPAFile file = do
    content <- readFile file
    let linesOfFile = lines content
    let result = map parseNgram $ parseLine linesOfFile
    return result
    where
        parseLine :: [String] -> [String]
        parseLine = filter (\x -> (head(x) /= '\\') && (take 5 x /= "ngram")) . filter (not . null)

-- | parse the .arpa file to a list of strings
parseARPAFile' :: String -> IO [String]
parseARPAFile' file = do
    content <- readFile file
    let linesOfFile = lines content
    let result = parseLine linesOfFile
    return result 
    where
        parseLine :: [String] -> [String]
        parseLine = filter (\x -> (head(x) /= '\\') && (take 5 x /= "ngram")) . filter (not . null)

-- | parse the text file to a string list with the position of the word occurrence
parseFile :: String -> IO [PosString]
parseFile file = do
    content <- readFile file
    let linesOfFile = lines content
    let result = parseLine 0 linesOfFile
    return result
    where
        parseLine :: Int -> [String] -> [PosString]
        parseLine _ [] = []
        parseLine acc (x:xs) = PosString acc 0 x : (parseLine (acc + 1) xs)

-- | return only consecutive token as list
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
-- Call this function with the wordlist of the already right length for the ngram
-- it will filter all of non matching length
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
