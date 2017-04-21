module DictCC.DictCC
    (
      Translation(..)
    , dictCC
    ) where

import Network.HTTP
import Text.HTML.TagSoup
import Text.Regex.Posix

-- Types/Data
type FromLang = String
type ToLang = String
type Lookup = String

data Translation = Translation
        { source :: String
        , target :: String
        , votes  :: Int
        }


-- dictCC: main program logic
-- Takes a source language, destination language, and a word
-- or phrase and returns a list of translations.
dictCC :: FromLang -> ToLang -> Lookup -> IO ([Translation])
dictCC from to word = do
    html <- searchWord word from to
    return $ translations ( toWords ( parseTags html))
    where
        translations :: [String] -> [Translation]
        translations [] = []
        translations (x:y:xs) =
            let (y', votes) = splitVotes y
            in Translation x y' votes : translations xs

        splitVotes :: String -> (String, Int)
        splitVotes s =
            case s =~~ "^([0-9]*) (.*)$" :: Maybe String of
                Just m  -> (concat . tail . words $ m, read . head . words $ m)
                Nothing -> (s, 0)

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
