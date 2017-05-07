import Data.List
import Data.Tuple (swap)

import DictCC.Config
import DictCC.DictCC
import DictCC.Output
import Options


-- Main CLI entry point
main :: IO ()
main = do
    config <- readConfig
    (options, word) <- getCliOpts $ defaultOptions config
    let langs = (optFromLang options, optToLang options)
    let (from, to) = if optReverse options
            then swap langs
            else langs
    results <- dictCC from to word
    printResults results (from, to) (optLimit options)
