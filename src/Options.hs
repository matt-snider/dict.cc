module Options
    (
      Options(..)
    , getCliOpts
    ) where

import System.Console.GetOpt
import System.Environment


-- Option definitions and defaults
data Options = Options
    { optReverse  :: Bool
    , optLimit    :: Int
    , optFromLang :: String
    , optToLang   :: String
    } deriving Show


-- CLI stuff
options :: [OptDescr (Options -> Options)]
options =
 [ Option ['f'] ["from"]
    (ReqArg (\f opts -> opts { optFromLang = f }) "FROM")
    "language to translate from"
 , Option ['t'] ["to"]
    (ReqArg (\t opts -> opts { optToLang = t }) "TO")
    "language to translate to"
 , Option ['l'] ["limit"]
    (ReqArg (\l opts -> opts { optLimit = read l }) "LIMIT")
    "limit the number of results"
 , Option ['r'] ["reverse"]
    (NoArg (\opts -> opts { optReverse = True }))
    "reverse the default from & to"
 ]


getCliOpts :: Options -> IO (Options, String)
getCliOpts defaultOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, w:[], []) -> return (foldl (flip id) defaultOptions o, w)
        (o, [], [])   -> ioError (userError ("\nprovide a word to lookup\n"))
        (_, w:ws, []) -> ioError (userError ("\nprovide only a single word to lookup\n\t"
                                              ++ (show $ length (w:ws)) ++ " found: " ++ show (w:ws)  ++ "\n"))
        (_,_,errs)    -> ioError (userError ("\n" ++ concat errs ++ usageInfo header options ++ "\n"))
    where header = "Usage: dict.cc [OPTION...] word"
