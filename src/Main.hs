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
    let maxFromLen = maximum $ map (length . source) trans
    let maxToLen = maximum $ map (length . target) trans
    let fmtStr = getFmtStr maxFromLen maxToLen

    printUTF $ printf fmtStr lheader rheader
    printUTF $ printf fmtStr (getUnderline lheader) (getUnderline rheader)
    mapM_ (\t -> printUTF (printf fmtStr (source t) (target t))) $  limitResults limit $ trans
    where
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
