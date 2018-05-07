{-# LANGUAGE NoOverloadedStrings #-}

module DictCC.DictCC
    ( Translation(..)
    , Results(..)
    , Category(..)
    , dictCC
    ) where


import Data.Char (isSpace)
import Data.List
import Data.Maybe (fromMaybe)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Simple
import Text.HTML.TagSoup
import Text.Read (readMaybe)
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


fromCategoryString :: String -> Category
fromCategoryString "Verben"      = Verb
fromCategoryString "Substantive" = Noun
fromCategoryString _             = Other


{---------------------
 - Main Entrypoint
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

parseHtml :: [Tag String] -> [Translation]
parseHtml tags =
    [ Translation
        (parseTransLeft translationRow)
        (parseTransRight translationRow)
        (parseVotes translationRow)
        (parseCategory sectionHeader)

    -- Break up into sections by word class
    | (sectionHeader, sectionContent) <-
        map breakSection $ partitions isSectionBoundary tags

    , translationRow <-
        partitions isTranslationRow sectionContent
    ]


-- Translation rows
-- Each translation is in a <tr id="trX"> where X are
-- consecutive numbers counting up from 1.
isTranslationRow :: Tag String -> Bool
isTranslationRow t =
    isTagOpenName "tr" t
    && fromAttrib  "id" t =~ "tr[[:digit:]]" :: Bool


isTranslationContent :: Tag String -> Bool
isTranslationContent = (~== "<td class=td7nl>")


parseTransLeft :: [Tag String] -> String
parseTransLeft = innerText . left
    where
        left = head . partitions isTranslationContent


parseTransRight :: [Tag String] -> String
parseTransRight = innerText . rightContent
    where
        rightContent = removeVotes . takeWhile (not . isTagCloseName "td") . right

        removeVotes tags = case findIndex isVotesDiv tags of
            Just x  -> drop (x + 2) tags
            Nothing -> tags

        right = last . partitions isTranslationContent


-- Categories
-- The translations are sectioned off according to
-- word class (e.g. Noun, Verb, Other)
isCategoryHeader :: Tag String -> Bool
isCategoryHeader = (~== "<td class=td6>")


isOtherHeader :: Tag String -> Bool
isOtherHeader = (~== "<td class=bluebar>")


isSectionBoundary :: Tag String -> Bool
isSectionBoundary t = isCategoryHeader t || isOtherHeader t


breakSection :: [Tag String] -> ([Tag String], [Tag String])
breakSection = break isTdClose


parseCategory :: [Tag String] -> Category
parseCategory = maybe Other fromCategoryString . categoryContent
    where
        categoryContent = maybeLast . words . innerText . categoryTags

        categoryTags = takeWhile (not . isTdClose) . dropWhile (not . isCategoryHeader)

        maybeLast [] = Nothing
        maybeLast xs = Just(last xs)


-- Votes
-- The number of votes, when presents, is contained
-- in a <div> with float:right inside the 'target'
-- of a translation
isVotesDiv :: Tag String -> Bool
isVotesDiv t =
    isTagOpenName "div" t && isFloatRight t


isFloatRight :: Tag String -> Bool
isFloatRight t = fromAttrib "style" t =~ "float:[[:blank:]]?right" :: Bool


parseVotes :: [Tag String] -> Int
parseVotes t = fromMaybe 0 (readMaybe $ votesText t)
    where
        votesText = innerText . take 2 . dropWhile (not . isVotesDiv)


extractWords :: [Tag String] -> String
extractWords =
        unwords .
        map (trim . fromTagText) .
        filter isTagText .
        takeWhile (~/= "</td>")


buildHeaders :: [Tag String] -> (String, String)
buildHeaders tags =
    let l = filter (not . null) $
            map (takeWhile (/= '»') . extractWords) $
            partitions (~== "<td class=td2>") tags
    in (head l, l !! 1)


-- Helper methods
isTdClose :: Tag String -> Bool
isTdClose = isTagCloseName "td"


trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


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

tuplify :: [a] -> [(a, a)]
tuplify [] = []
tuplify xs =
    let (ys, zs) = splitAt 2 xs
    in (head ys, ys !! 1) : tuplify zs
