{-# LANGUAGE BangPatterns #-}

module Main where

import System.Environment (getArgs)
import Data.List 
import Data.Char
import Data.Function
import Data.Typeable
import Text.Read (readMaybe)

data Ngram = Ngram { weight :: Double, ngram :: [String], fallback :: Maybe Double}  deriving (Show)

-- | Usage: ./wordpred <numberwords> <ngrammodel> <textfile> <line> <column>
main :: IO ()
main = do
    args <- getArgs
    contentarpa <- readFile (args !! 1) 
    let tralala = parseARPAFile contentarpa 
    content <- readFile (args !! 2)
    let count = read (args !! 0) :: Int
    let query = parseFile 5 ((read (args !! 3) :: Int) - 1) ((read (args !! 4) :: Int) - 1) content 
    let result = take count (compareNgram count tralala query)
    mapM_ putStrLn $ (map output result)
    putStrLn "--END--"

-- | append one list to another without (++)
append :: [a] -> [a] -> [a]
append xs ys = foldr (\x y -> x:y) ys xs

output :: (String, Double) -> String
output (a,b) = a ++ "   " ++ (show b)

-- | return only consecutive token as list
parseString :: String -> [String]
parseString = filter (not . null) . map trim . (groupBy ((==) `on` isAlpha))
    where
        trim :: String -> String
        trim = f . f
        f = reverse . dropWhile isSpace

-- | generate a ngram from a string
parseNgram :: String -> Ngram
parseNgram s = go (words s)
    where
        -- | if n = k there is no fallback weight -> set it to 0
        go :: [String] -> Ngram
        go xs 
          | length xs < 3 = Ngram 0.0 [] Nothing
          | isDouble lastword = Ngram (read $ head xs) (init $ tail xs) lastword
          | otherwise = Ngram (read $ head xs) (tail xs) lastword
            where
                lastword = readMaybe (last xs) :: Maybe Double
                
                isDouble :: Maybe Double -> Bool
                isDouble Nothing = False
                isDouble (Just a) = True       

-- | parse the .arpa file to a list of ngrams
parseARPAFile :: String -> [Ngram]
parseARPAFile file = map (parseNgram) $ parseLine (lines file)
    where
        parseLine :: [String] -> [String]
        parseLine = filter (\x -> (head x /= '\\') && (take 5 x /= "ngram")) . filter (not . null)

-- | returns the k predecessing words (◔_◔)
parseFile :: Int -> Int -> Int -> String -> [String]
parseFile k lin col file = parseLine (lines file)
    where
        parseLine :: [String] -> [String]
        parseLine ls = take' k (go lin (parseLine' col (ls !! lin)) ls)

        parseLine' :: Int -> String -> [String]
        parseLine' c l 
          | c == length l = words l
          | isSpace (l !! c) = words (take c l)
          | otherwise = init $ words (take c l)

        go :: Int -> [String] -> [String] -> [String]
        go l ws ls
          | l < 0 = []
          | l == 0 = ws
          | length ws + length prevline < k = append (go (l-1) prevline ls) ws
          | length ws >= k = ws
          | otherwise = append prevline ws
            where
                prevline = parseLine' (length (ls !! (l-1))) (ls !! (l-1))

        take' :: Int -> [a] -> [a]
        take' e = reverse . take e . reverse

-- | calls compareNgram' until the number of predictions is reached
compareNgram :: Int -> [Ngram] -> [String] -> [(String, Double)]
compareNgram k ns cs
  | cs == [] = []
  | length ngrams < k = append ngrams (compareNgram (k - length ngrams) ns (tail cs))
  | otherwise = ngrams
    where
        ngrams = top . group $ compareNgram' ns cs
        
        group :: [Ngram] -> [(String, Double)]
        group = group'' . map group'
            
        group' :: Ngram -> (String, Double)
        group' (Ngram a xs _) = (last xs, a)

        group'' :: [(String, Double)] -> [(String, Double)]
        group'' [] = []
        group'' ((a,b):xs) = case index of
                               Nothing -> (a,b) : (group'' xs)
                               Just p -> group'' $ replaceNth p (a, b - snd (xs !! p)) xs
            where
                index = elemIndex a (map fst xs)

                replaceNth :: Int -> (String, Double) -> [(String, Double)] -> [(String, Double)]
                replaceNth n newVal (x:xs)
                  | n == 0 = newVal:xs
                  | otherwise = x:replaceNth (n-1) newVal xs

        top :: Ord b => [(a,b)] -> [(a,b)]
        top xs = reverse $ sortBy (compare `on` snd) xs

-- | takes a list of ngrams and a list of strings of consecutive occuring words
-- and returns all ngrams matching those words
-- Note that the last word of each ngram is cut in order to predict the next word
-- Call this function with the wordlist of the already right length for the ngram
-- it will filter all of non matching length
compareNgram' :: [Ngram] -> [String] -> [Ngram]
compareNgram' ns cs = matching (filter (\n -> length (ngram n) - 1 == length cs) ns) cs 
    where
        matching :: [Ngram] -> [String] -> [Ngram]
        matching [] _ = []
        matching _ [] = [] 
        matching (n:ns) cs 
          | areEqualNS (reduce n) cs = n : matching ns cs
          | otherwise = matching ns cs        
        
        reduce :: Ngram -> Ngram
        reduce (Ngram a xs Nothing) = Ngram a (take (length xs - 1) xs) Nothing
        reduce (Ngram a xs (Just b)) = Ngram (a + b) (take (length xs - 1) xs) Nothing

        areEqualNS :: Ngram -> [String] -> Bool
        areEqualNS (Ngram _ s1 _) s2 = s1 == s2
