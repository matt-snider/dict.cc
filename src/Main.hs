import Data.Tuple (swap)

import DictCC.Config
import DictCC.DictCC
import DictCC.Output
import Options

-- Version
import Paths_dict_cc (version)
import Data.Version (showVersion)


-- Main CLI entry point
main :: IO ()
main = do
    config <- readConfig
    (options, word) <- getCliOpts $ defaultOptions config
    case options of
        ShowHelp    -> putStr usage
        ShowVersion -> putStrLn ("dict.cc version " ++ showVersion version)
        _           -> do
            let langs = (optFromLang options, optToLang options)
            let (from, to) = if optReverse options
                    then swap langs
                    else langs
            results <- (filterTrans options) <$> dictCC from to word
            printResults results (optLimit options)
    where
        filterTrans :: Options -> Results -> Results
        filterTrans Options{optIsNoun = True} results =
            replace results [ x | x@Translation{category = Noun} <- translations results ]

        filterTrans Options{optIsVerb = True} results =
            replace results [ x | x@Translation{category = Verb} <- translations results ]

        filterTrans _ results = results

        replace :: Results -> [Translation] -> Results
        replace r ts = r { translations = ts }
