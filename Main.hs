import Network.HTTP
import Text.Printf
import Text.HTML.TagSoup
import System.Environment
import System.Console.GetOpt

-- Flag definitions
data Flag = From String | To String | IsNoun | IsVerb deriving Show

options :: [OptDescr Flag]
options =
 [ Option ['f'] ["from"] (ReqArg From "FROM") "language to translate from"
 , Option ['t'] ["to"]   (ReqArg To "TO")     "language to translate to"
 , Option ['n'] ["noun"] (NoArg IsNoun)       "the word is a noun"
 , Option ['v'] ["verb"] (NoArg IsVerb)       "the word is a verb"
 ]


getCliOpts :: IO ([Flag], String)
getCliOpts = do
    args <- getArgs
    case getOpt Permute options args of
        (o, w:[], []) -> return (o, w)
        (o, [], [])   -> ioError (userError ("\nprovide a word to lookup\n"))
        (_, w:ws, []) -> ioError (userError ("\nprovide only a single word to lookup\n\t"
                                              ++ (show $ length (w:ws)) ++ " found: " ++ show (w:ws)  ++ "\n"))
        (_,_,errs)    -> ioError (userError ("\n" ++ concat errs ++ usageInfo header options ++ "\n"))
    where header = "Usage: dict.cc [OPTION...] word"


-- Main logic
dictCC :: IO ()
dictCC = do 
    args <- getArgs
    tags <- parseTags <$> searchWord (args !! 0)
    let words = 
            map f $ 
            partitions (~== "<td class=td7nl>") tags
    let wordLens = map length words
    let maxEnLen = maximum $ oddElems wordLens
    let maxDeLen = maximum $ evenElems wordLens
    let fmtStr = getFmtStr maxEnLen maxDeLen
    printf fmtStr "English" "German"
    printf fmtStr "=========" "======="
    mapM_ (\(a, b) -> printf fmtStr a b) $ take 10 $ tuplify words
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

