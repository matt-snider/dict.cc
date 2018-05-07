module DictCC.DictCC
    ( Translation(..)
    , Results(..)
    , Category(..)
    , dictCC
    ) where


import Prelude hiding (words)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text, replace, strip, unpack, words)
import qualified Data.Text.Lazy.Encoding as Encoding
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Simple
import Text.HTML.TagSoup
import Text.Read (readMaybe)
import Text.Regex.Posix


{--------------
 - Types/Data -
 --------------}
type FromLang = Text
type ToLang   = Text
type Lookup   = Text

data Results = Results
        { translations :: [Translation]
        , fromHeader   :: Text
        , toHeader     :: Text
        }

data Translation = Translation
        { source   :: Text
        , target   :: Text
        , votes    :: Int
        , category :: Category
        }

data Category
    = Verb
    | Noun
    | Other
    deriving Show


fromCategoryString :: Text -> Category
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
    let headers = parseLanguages tags
    return $ maybeReverse Results
        { translations = parseHtml tags
        , fromHeader = fst headers
        , toHeader = snd headers
        } from to


{--------
 - HTTP -
 --------}

-- Make an HTTP call and retrieve the resulting html page
searchWord :: Text -> Text -> Text -> IO Text
searchWord word from to =
    let
        request = parseRequest_
                $  "https://"
                ++ (unpack from) ++ "-" ++ (unpack to)
                ++ ".dict.cc/?s="
                ++ urlEncode (unpack word)
    in
        fmap (Encoding.decodeUtf8 . getResponseBody) (httpLBS request)


{----------------
 - HTML Parsing -
 ----------------}

parseHtml :: [Tag Text] -> [Translation]
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
isTranslationRow :: Tag Text -> Bool
isTranslationRow t =
    isTagOpenName "tr" t
    && unpack (fromAttrib "id" t) =~ ("tr[[:digit:]]" :: String) :: Bool


isTranslationContent :: Tag Text -> Bool
isTranslationContent = (~=== "<td class=td7nl>")


parseTransLeft :: [Tag Text] -> Text
parseTransLeft = innerText . left
    where
        left = head . partitions isTranslationContent


parseTransRight :: [Tag Text] -> Text
parseTransRight = innerText . rightContent
    where
        rightContent = removeVotes . List.takeWhile (not . isTagCloseName "td") . right

        removeVotes tags = case List.findIndex isVotesDiv tags of
            Just x  -> drop (x + 2) tags
            Nothing -> tags

        right = last . partitions isTranslationContent


-- Categories
-- The translations are sectioned off according to
-- word class (e.g. Noun, Verb, Other)
isCategoryHeader :: Tag Text -> Bool
isCategoryHeader = (~=== "<td class=td6>")


isOtherHeader :: Tag Text -> Bool
isOtherHeader = (~=== "<td class=bluebar>")


isSectionBoundary :: Tag Text -> Bool
isSectionBoundary t = isCategoryHeader t || isOtherHeader t


breakSection :: [Tag Text] -> ([Tag Text], [Tag Text])
breakSection = break isTdClose


parseCategory :: [Tag Text] -> Category
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
isVotesDiv :: Tag Text -> Bool
isVotesDiv t =
    isTagOpenName "div" t && isFloatRight t


isFloatRight :: Tag Text -> Bool
isFloatRight t = unpack (fromAttrib "style" t) =~ ("float:[[:blank:]]?right" :: String) :: Bool


parseVotes :: [Tag Text] -> Int
parseVotes t = fromMaybe 0 (readMaybe $ votesText t)
    where
        votesText = unpack . innerText . take 2 . dropWhile (not . isVotesDiv)


-- extractWords :: [Tag Text] -> Text
-- extractWords =
--         unwords .
--         map ( . fromTagText) .
--         filter isTagText .
--         takeWhile (~/= "</td")

-- Language Labels
-- Each column has a header describing which
-- language is shown below it.
-- We parse these from the document instead of hardcoding
-- them in the program.
-- These are contained in a <td class="td2"> elem
isLanguageHeader :: Tag Text -> Bool
isLanguageHeader = (~=== "<td class=td2 dir=ltr>")


parseLanguages :: [Tag Text] -> (Text, Text)
parseLanguages tags =
    let headers = map content $ extractHeaders tags
    in (head headers, headers !! 1)
    where
        content = clean . head . words . innerText . takeWhile (not . isTdClose)

        clean = strip . replace "»" ""

        extractHeaders = take 2 . partitions isLanguageHeader


-- Helper methods
isTdClose :: Tag Text -> Bool
isTdClose = isTagCloseName "td"

-- Override TagSoup's ~== so we don't have issues with
-- OverloadedStrings and ambiguity
(~===) :: Tag Text -> String -> Bool
(~===) a b = a ~== (b :: String)

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
