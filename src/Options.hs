module Options
    (
      Options(..)
    , getCliOpts
    , usage
    ) where

import Control.Exception
import System.Console.GetOpt
import System.Environment

-- Option definitions and defaults
data Options =
    ShowHelp |
    ShowVersion |
    Options
    { optReverse  :: Bool
    , optLimit    :: Int
    , optFromLang :: String
    , optToLang   :: String
    , optIsNoun   :: Bool
    , optIsVerb   :: Bool
    } deriving (Eq, Show)


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
 , Option ['n'] ["noun"]
    (NoArg (\opts -> opts { optIsNoun = True, optIsVerb = False }))
    "only return results that are nouns"
 , Option ['v'] ["verb"]
    (NoArg (\opts -> opts { optIsVerb = True, optIsNoun = False }))
    "only return results that are verbs"
 , Option ['h'] ["help"]
    (NoArg (\_ -> ShowHelp))
    "show this help text"
 , Option ['V'] ["version"]
    (NoArg (\_ -> ShowVersion))
    "show the version"
 ]

-- Errors
data CLIError = NoLookupProvided

instance Exception CLIError

instance Show CLIError where
    show NoLookupProvided = "provide a word to lookup\n" ++ usage


usage :: String
usage = usageInfo "Usage: dict.cc [OPTION...] word\n" options


-- Option parsing
getCliOpts :: Options -> IO (Options, String)
getCliOpts defaultOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, w:[], []) -> return (foldl (flip id) defaultOptions o, w)
        (_, w:ws, []) -> throw NoLookupProvided
        (o, [], [])   ->
            if contains o ShowHelp then
                return (ShowHelp, "")
            else if contains o ShowVersion then
                return (ShowVersion, "")
            else
                throw NoLookupProvided

        (_,_,errs)    -> ioError (userError ("\n" ++ concat errs ++ usage ++ "\n"))
    where
        contains :: [Options -> Options] -> Options -> Bool
        contains [] _ = False
        contains (x:xs) o = x Options{} == o || (contains xs o)