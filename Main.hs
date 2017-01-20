import Text.Printf
import Network.HTTP
import Text.HTML.TagSoup


dictCC :: IO ()
dictCC = do 
    tags <- parseTags <$> searchWord "bier"
    let trans = 
            tuplify $ 
            map f $ 
            partitions (~== "<td class=td7nl>") tags
    putStrLn "English                       German"
    putStrLn "===========                   =========="
    mapM_ (\(a, b) -> printf "%s                            %s\n" a b) trans
    where
        f :: [Tag String] -> String
        f =  unwords . 
             map fromTagText . 
             filter isTagText . 
             takeWhile (~/= "</td>")


searchWord :: String -> IO String
searchWord w = 
    getResponseBody =<< simpleHTTP 
        (getRequest $ "http://www.dict.cc/?s=" ++ w)


tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify xs = 
    let (ys, zs) = splitAt 2 xs
    in (ys !! 0, ys !! 1) : tuplify zs


main :: IO ()
main = dictCC

