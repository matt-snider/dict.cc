module Options
    (
      Options(..)
    , getCliOpts
    ) where

import Control.Exception
import System.Console.GetOpt
import System.Environment

-- Version
import Paths_dict_cc (version)
import Data.Version (showVersion)

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
              | ShowCLIHelp
              | ShowCLIVersion

instance Exception CLIError

instance Show CLIError where
    show NoLookupProvided = withUsage "provide a word to lookup"
    show ShowCLIHelp      = withUsage "help"
    show ShowCLIVersion   = "version " ++ showVersion version


withUsage :: String -> String
withUsage msg = msg
    ++ usageInfo "\n\nUsage: dict.cc [OPTION...] word\n" options


getCliOpts :: Options -> IO (Options, String)
getCliOpts defaultOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, w:[], []) -> return (foldl (flip id) defaultOptions o, w)
        (_, w:ws, []) -> throw NoLookupProvided
        (o, [], [])   ->
            if contains o ShowHelp then
                throw ShowCLIHelp
            else if contains o ShowVersion then
                throw ShowCLIVersion
            else
                throw NoLookupProvided

        (_,_,errs)    -> ioError (userError ("\n" ++ concat errs ++ withUsage "" ++ "\n"))
    where
        contains :: [Options -> Options] -> Options -> Bool
        contains [] _ = False
        contains (x:xs) o = x Options{} == o || (contains xs o)