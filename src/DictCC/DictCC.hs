module DictCC.DictCC
    (
      Translation(..)
    , Results(..)
    , Category(..)
    , dictCC
    ) where

import Network.HTTP.Base (urlEncode)
import Network.HTTP.Simple
import Text.HTML.TagSoup
import Text.Regex.Posix
import qualified Data.ByteString.Lazy.Char8 as Char8


{--------------
 - Types/Data -
 --------------}
type FromLang = String
type ToLang = String
type Lookup = String

data Results = Results
        { translations :: [Translation]
        , fromHeader   :: String
        , toHeader     :: String
        }

data Translation = Translation
        { source   :: String
        , target   :: String
        , votes    :: Int
        , category :: Category
        }

data Category
    = Verb
    | Noun
    | Other
    deriving Show


{---------------------
 - Main lookup logic -
 ---------------------}

-- Takes a source language, destination language, and a word
-- or phrase and returns a list of translations.
dictCC :: FromLang -> ToLang -> Lookup -> IO Results
dictCC from to word = do
    html <- searchWord word from to
    let tags = parseTags html
    let headers = buildHeaders tags
    return $ maybeReverse Results
        { translations = parseHtml tags
        , fromHeader = fst headers
        , toHeader = snd headers
        } from to


{--------
 - HTTP -
 --------}

-- Make an HTTP call and retrieve the resulting html page
searchWord :: String -> String -> String -> IO String
searchWord word from to =
    let
        request = parseRequest_
                $  "https://"
                ++ from ++ "-" ++ to
                ++ ".dict.cc/?s="
                ++ urlEncode word
    in
        fmap
            (Char8.unpack . getResponseBody)
            (httpLBS request)


{----------------
 - HTML Parsing -
 ----------------}

-- View the list of translations as a list of groups
-- where each group contains a header indicating what
-- type of translations follow (e.g. Substantive, Verben, etc)
parseHtml :: [Tag String] -> [Translation]
parseHtml tags =
    [ Translation
        src
        trg
        votes
        (getCategory $ extractWords group)

    | (group, results)
        <- map (break $ isTagCloseName "td")
        $ partitions (\t -> t ~== "<td class=bluebar>" || t ~== "<td  class=td6>") tags

    , (src, trg, votes)
        <- map withVotes $ tuplify $ toWords results
    ]
    where
        withVotes :: (String, String) -> (String, String, Int)
        withVotes t = case snd t =~~ "^([0-9]*) (.*)$" :: Maybe String of
            Just m  ->
                ( fst t
                , concat . tail . words $ m
                , read . head . words $ m
                )
            Nothing ->
                ( fst t
                , snd t
                , 0
                )

        getCategory :: String -> Category
        getCategory s =
            case last . words $ s of
                "Verben" -> Verb
                "Substantive" -> Noun
                _ -> Other

        toWords :: [Tag String] -> [String]
        toWords = map
            extractWords .
            partitions (~== "<td class=td7nl>")

buildHeaders :: [Tag String] -> (String, String)
buildHeaders tags =
    let l = filter (not . null) $
            map (takeWhile (/= '»') . extractWords) $
            partitions (~== "<td class=td2>") tags
    in (head l, l !! 1)


{-----------------
 - Helpers/Utils -
 -----------------}

-- Dict.cc does not arrange columns based on the
-- searched languages (from, to), but rather based on the
-- reverse lexicographical order of their 2-letter codes.
maybeReverse :: Results -> FromLang -> ToLang -> Results
maybeReverse results from to
    | order == LT = doReverse results
    | otherwise   = results
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

extractWords :: [Tag String] -> String
extractWords =
        trimWhitespace .
        unwords .
        map fromTagText .
        filter isTagText .
        takeWhile (~/= "</td>")

    where
        trimWhitespace = unwords . words

tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify xs =
    let (ys, zs) = splitAt 2 xs
    in (head ys, ys !! 1) : tuplify zs
