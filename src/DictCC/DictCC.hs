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

defaultTranslation = Translation "" "" 0


-- dictCC: main program logic
-- Takes a source language, destination language, and a word
-- or phrase and returns a list of translations.
dictCC :: FromLang -> ToLang -> Lookup -> IO (Results)
dictCC from to word = do
    html <- searchWord word from to
    let tags = parseTags html
    let headers = buildHeaders tags
    let pairs = tuplify (toWords tags)
    let trans = map (buildTranslation
                     [ getVotes ])
                     pairs
    return $ maybeReverse Results
        { translations = trans
        , fromHeader = fst headers
        , toHeader = snd headers
        } from to
    where
        buildHeaders :: [Tag String] -> (String, String)
        buildHeaders tags =
            let l = filter ((>0) . length) $
                    map (takeWhile (/= '»')) $
                    map extractWords $
                    partitions (~== "<td class=td2>") tags
            in (l !! 0, l !! 1)

        buildTranslation :: [(Translation -> Translation)] -> (String, String) -> Translation
        buildTranslation ts (src, targ) =
            foldl (flip id) (defaultTranslation { source = src, target = targ }) ts

        getVotes :: Translation -> Translation
        getVotes t = case (target t) =~~ "^([0-9]*) (.*)$" :: Maybe String of
                        Just m  -> t { votes = read . head . words $ m
                                     , target = concat . tail . words $ m
                                     }
                        Nothing -> t


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


-- Other util functions
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


tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify xs =
    let (ys, zs) = splitAt 2 xs
    in (ys !! 0, ys !! 1) : tuplify zs
