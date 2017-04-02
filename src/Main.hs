import Data.List
import Network.HTTP
import Text.Printf
import Text.HTML.TagSoup
import System.Environment
import System.Console.GetOpt
import qualified Data.ByteString.Char8 as BS (putStr, pack)

-- Flag definitions
data Flag = Reverse
          | Limit String
          | From String
          | To String
          deriving (Show, Eq)

options :: [OptDescr Flag]
options =
 [ Option ['f'] ["from"]      (ReqArg From "FROM")     "language to translate from"
 , Option ['t'] ["to"]        (ReqArg To "TO")         "language to translate to"
 , Option ['l'] ["limit"]     (ReqArg Limit "LIMIT")   "limit the number of results"
 , Option ['r'] ["reverse"]   (NoArg Reverse)          "reverse the default from & to"
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


defaultTo = "de"
defaultFrom = "en"
defaultLimit = 0

isFrom :: Flag -> Bool
isFrom (From _) = True
isFrom _ = False


isTo :: Flag -> Bool
isTo (To _) = True
isTo _ = False

isLimit :: Flag -> Bool
isLimit (Limit _) = True
isLimit _ = False


getFrom :: [Flag] -> String
getFrom opts =
    if Reverse `elem` opts
        then _getTo opts
        else _getFrom opts

_getFrom opts =
    case find (isFrom) opts of
        Nothing -> defaultFrom
        Just (From f) -> f

getTo :: [Flag] -> String
getTo opts =
    if Reverse `elem` opts
        then _getFrom opts
        else _getTo opts

_getTo opts =
    case find (isTo) opts of
        Nothing -> defaultTo
        Just (To t) -> t

getLimit :: [Flag] -> Int
getLimit opts =
    case find isLimit opts of
        Nothing -> defaultLimit
        Just (Limit l) -> read l :: Int


-- Main logic
dictCC :: IO ()
dictCC = do
    (options, word) <- getCliOpts

    let from = getFrom options
    let to = getTo options
    let limit = getLimit options
    tags <- parseTags <$> searchWord word from to
    printResult (words tags) (headers tags) limit
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
