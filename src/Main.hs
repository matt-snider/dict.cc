import Data.List
import Data.Tuple (swap)
import qualified Data.ByteString.Char8 as BS (putStr, pack)
import Text.Printf

import DictCC.DictCC
import Options


-- Main CLI entry point
main :: IO ()
main = do
    (options, word) <- getCommand
    let langs = (optFromLang options, optToLang options)
    let (from, to) = if optReverse options
            then swap langs
            else langs
    results <- dictCC from to word
    printResults results [from, to] (optLimit options)


-- Output logic
type Header = String
type Limit = Int

printResults :: [Translation] -> [Header] -> Limit  -> IO ()
printResults [] _ _ = do
    putStrLn "No translations found."
printResults trans headers limit = do
    let (lheader, rheader) = (headers !! 0, headers !! 1)
    let wordLens = map length $ flatten trans
    let maxEnLen = maximum $ oddElems wordLens
    let maxDeLen = maximum $ evenElems wordLens
    let fmtStr = getFmtStr maxEnLen maxDeLen
    printUTF $ printf fmtStr lheader rheader
    printUTF $ printf fmtStr (getUnderline lheader) (getUnderline rheader)
    mapM_ (\t -> printUTF (printf fmtStr (source t) (target t))) $  limitResults limit $ trans
    where
        flatten :: [Translation] -> [String]
        flatten [] = []
        flatten ((Translation x y):xs) = x : y : flatten xs

        limitResults :: Int -> [a] -> [a]
        limitResults limit xs =
                case limit of
                  0 -> xs
                  x -> take x xs

        getFmtStr :: Int -> Int -> String
        getFmtStr left right = printf "%%-%ds %%%ds\n" left right

        getUnderline :: String -> String
        getUnderline w =
                concat $
                take wordLength $
                repeat "="
            where wordLength = (length w) + 1

        printUTF :: String -> IO ()
        printUTF = BS.putStr . BS.pack





tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify xs =
    let (ys, zs) = splitAt 2 xs
    in (ys !! 0, ys !! 1) : tuplify zs


oddElems :: [a] -> [a]
oddElems [] = []
oddElems (x:xs) = x : (oddElems $ drop 1 xs)


evenElems :: [a] -> [a]
evenElems [] = []
evenElems (x:[]) = []
evenElems (x:xs) = xs !! 0 : (evenElems $ drop 1 xs)
