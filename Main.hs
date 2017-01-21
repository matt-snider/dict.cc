import Text.Printf
import Network.HTTP
import Text.HTML.TagSoup


dictCC :: IO ()
dictCC = do 
    tags <- parseTags <$> searchWord "bier"
    let words = 
            map f $ 
            partitions (~== "<td class=td7nl>") tags
    let wordLens = map length words
    let maxEnLen = maximum $ oddElems wordLens
    let maxDeLen = maximum $ evenElems wordLens
    let fmtStr = getFmtStr maxEnLen maxDeLen
    printf fmtStr "English" "German"
    printf fmtStr "=========" "======="
    mapM_ (\(a, b) -> printf fmtStr a b) $ tuplify words
    where
        f :: [Tag String] -> String
        f =  trimWhitespace .
             unwords .
             map fromTagText . 
             filter isTagText . 
             takeWhile (~/= "</td>")
        
        getFmtStr :: Int -> Int -> String
        getFmtStr left right = printf "%%-%ds %%%ds\n" left right


trimWhitespace :: String -> String
trimWhitespace = unwords . words



searchWord :: String -> IO String
searchWord w = 
    getResponseBody =<< simpleHTTP 
        (getRequest $ "http://www.dict.cc/?s=" ++ w)


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

