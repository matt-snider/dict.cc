module DictCC.DictCC
    (
      Translation(..)
    , Results(..)
    , dictCC
    ) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.Regex.Posix

-- Types/Data
type FromLang = String
type ToLang = String
type Lookup = String


data Results = Results
        { translations :: [Translation]
        , fromHeader   :: String
        , toHeader     :: String
        }

data Translation = Translation
        { source :: String
        , target :: String
        , votes  :: Int
        }


-- dictCC: main program logic
-- Takes a source language, destination language, and a word
-- or phrase and returns a list of translations.
dictCC :: FromLang -> ToLang -> Lookup -> IO (Results)
dictCC from to word = do
    html <- searchWord word from to
    let tags = parseTags html
    let trans = toTranslations (toWords tags)
    let headers = getHeaders tags
    return $ maybeReverse Results
        { translations = trans
        , fromHeader = fst headers
        , toHeader = snd headers
        } from to
    where
        toTranslations :: [String] -> [Translation]
        toTranslations [] = []
        toTranslations (x:y:xs) =
            let (y', votes) = splitVotes y
            in Translation x y' votes : toTranslations xs

        splitVotes :: String -> (String, Int)
        splitVotes s =
            case s =~~ "^([0-9]*) (.*)$" :: Maybe String of
                Just m  -> (concat . tail . words $ m, read . head . words $ m)
                Nothing -> (s, 0)

        getHeaders :: [Tag String] -> (String, String)
        getHeaders tags =
            let l = filter ((>0) . length) $
                    map (takeWhile (/= '»')) $
                    map extractWords $
                    partitions (~== "<td class=td2>") tags
            in (l !! 0, l !! 1)

        toWords :: [Tag String] -> [String]
        toWords = map
            extractWords .
            partitions (~== "<td class=td7nl>")

        extractWords :: [Tag String] -> String
        extractWords =
             trimWhitespace .
             unwords .
             map fromTagText .
             filter isTagText .
             takeWhile (~/= "</td>")

        trimWhitespace :: String -> String
        trimWhitespace = unwords . words


-- Make an HTTP call and retrieve the resulting html page
searchWord :: String -> String -> String -> IO String
searchWord word from to =
    getResponseBody =<< simpleHTTP
        (getRequest $ "http://" ++ from ++ "-" ++ to ++ ".dict.cc/?s=" ++ urlEncode word)


-- Note that dict-cc does not arrange columns based on the
-- searched languages (from, to), but rather based on the
-- reverse lexicographical order of their 2-letter codes.
maybeReverse :: Results -> FromLang -> ToLang -> Results
maybeReverse results from to
    | order `elem` [LT] = doReverse results
    | otherwise         = results
    where
        order = compare from to

        doReverse :: Results -> Results
        doReverse results = results
            { fromHeader   = toHeader results
            , toHeader     = fromHeader results
            , translations = map flip $ translations results
            }

        flip :: Translation -> Translation
        flip trans = trans
            { source = target trans
            , target = source trans
            }
