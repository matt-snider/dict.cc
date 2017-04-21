{-# LANGUAGE OverloadedStrings #-}

module DictCC.Output
    (
      printResults
    ) where

import Data.Text.Encoding as T (decodeUtf8)
import Data.ByteString.Char8 as BS (unpack, pack)
import Data.Text.Format as F

import DictCC.DictCC


type Header = String
type ColumnWidth = Int
type HeaderDescription = (Header, ColumnWidth)
type Limit = Int


printResults :: [Translation] -> (Header, Header) -> Limit  -> IO ()
printResults [] _ _ = putStrLn "No translations found."

printResults trans headers limit = do
    let maxFromLen = maximum $ map (length . source) trans
    let maxToLen = maximum $ map (length . target) trans
    printHeaders ((fst headers, maxFromLen), (snd headers, maxToLen))
    mapM_ (printResult (maxFromLen, maxToLen)) (limitResults limit $ trans)
    where
        limitResults :: Int -> [a] -> [a]
        limitResults limit xs =
                case limit of
                  0 -> xs
                  x -> take x xs


-- Print a translation
printResult :: (ColumnWidth, ColumnWidth) -> Translation -> IO ()
printResult (toLen, frLen)  (Translation from to vote) =
            F.print "{} {}\n" (right frLen  ' ' (decodeUtf8 $ BS.pack from),
                               left  toLen  ' ' (decodeUtf8 $ BS.pack to))


-- Prints the headers and the underline taking
printHeaders :: (HeaderDescription, HeaderDescription) -> IO ()
printHeaders (frHeader, toHeader) =
        let (fr, frLen) = frHeader
            (to, toLen) = toHeader
            frUnderline = underline $ length fr + 3
            toUnderline = underline $ length to + 3
        in  F.print "{} {}\n"
                (right frLen ' ' fr,
                 left  toLen ' ' to)
        >>  F.print "{} {}\n"
                (right frLen ' ' frUnderline,
                 left  toLen ' ' toUnderline)


-- Creates a header underline of specified length
underline :: Int -> String
underline = flip take $ repeat '='