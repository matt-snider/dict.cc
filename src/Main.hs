import Data.List
import Data.Tuple (swap)
import Network.HTTP
import Text.Printf
import Text.HTML.TagSoup
import qualified Data.ByteString.Char8 as BS (putStr, pack)

import Options


-- Main logic
dictCC :: IO ()
dictCC = do
    (options, word) <- getCliOpts
    let langs = (optFromLang options, optToLang options)
    let (from, to) = if optReverse options
            then swap langs
            else langs
    tags <- parseTags <$> searchWord word from to
    printResult (words tags) (headers tags) (optLimit options)
    where
        extractWords :: [Tag String] -> String
        extractWords =
             trimWhitespace .
             unwords .
             map fromTagText .
             filter isTagText .
             takeWhile (~/= "</td>")

        words :: [Tag String] -> [String]
        words tags =
            map extractWords $
            partitions (~== "<td class=td7nl>") tags

        headers :: [Tag String] -> [String]
        headers tags =
            take 2 $
            filter ((>0) . length) $
            map (takeWhile (/= '»')) $
            map extractWords $
            partitions (~== "<td class=td2>") tags



printResult :: [String] -> [String] -> Int -> IO ()
printResult [] _ _ = do
    putStrLn "No translations found."
printResult words headers limit = do
    let (lheader, rheader) = (headers !! 0, headers !! 1)
    let wordLens = map length words
    let maxEnLen = maximum $ oddElems wordLens
    let maxDeLen = maximum $ evenElems wordLens
    let fmtStr = getFmtStr maxEnLen maxDeLen
    printUTF $ printf fmtStr lheader rheader
    printUTF $ printf fmtStr (getUnderline lheader) (getUnderline rheader)
    mapM_ (\(a, b) -> printUTF (printf fmtStr a b)) $  limitResults limit $ tuplify words
    where
        limitResults :: Int -> [(a, a)] -> [(a, a)]
        limitResults limit words =
                case limit of
                  0 -> words
                  x -> take x words

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


trimWhitespace :: String -> String
trimWhitespace = unwords . words



searchWord :: String -> String -> String -> IO String
searchWord word from to =
    getResponseBody =<< simpleHTTP
        (getRequest $ "http://" ++ from ++ "-" ++ to ++ ".dict.cc/?s=" ++ urlEncode word)


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


main :: IO ()
main = dictCC
