module DictCC.Output
    ( printResults
    ) where

import Prelude hiding (length, maximum, replicate)
import Data.Int
import qualified Data.List as List
import Data.Text.Lazy
import Data.Text.Format as F

import DictCC.DictCC


type Header = Text
type ColumnWidth = Int64
type HeaderDescription = (Header, ColumnWidth)
type Limit = Int


printResults :: Results -> Limit  -> IO ()
printResults Results{ translations = [] } _ = putStrLn "No translations found."

printResults results limit = do
    let limitedTrans = limitResults limit $ translations results
    let maxFromLen = List.maximum $ List.map (length . source) limitedTrans
    let maxToLen = List.maximum $ List.map (length . target) limitedTrans
    printHeaders ((fromHeader results, maxFromLen), (toHeader results, maxToLen))
    mapM_ (printResult (maxFromLen, maxToLen)) limitedTrans
    where
        limitResults :: Int -> [a] -> [a]
        limitResults limit xs =
                case limit of
                  0 -> xs
                  x -> List.take x xs


-- Print a translation
printResult :: (ColumnWidth, ColumnWidth) -> Translation -> IO ()
printResult (toLen, frLen)  (Translation from to vote _) =
            let votes = if vote == 0 then (pack "") else (pack $ " [" ++ show vote ++ " \10003]")
            in  F.print "{} {}{}\n"
                (justifyLeft frLen ' ' from,
                 justifyRight  (toLen - length votes)' ' to,
                 votes)


-- Prints the headers and the underline taking
printHeaders :: (HeaderDescription, HeaderDescription) -> IO ()
printHeaders (frHeader, toHeader) =
        let (fr, frLen) = frHeader
            (to, toLen) = toHeader
            frUnderline = underline $ length fr + 3
            toUnderline = underline $ length to + 3
        in  F.print "{} {}\n"
                (justifyLeft frLen ' ' fr,
                 justifyRight  toLen ' ' to)
        >>  F.print "{} {}\n"
                (justifyLeft frLen ' ' frUnderline,
                 justifyRight  toLen ' ' toUnderline)


-- Creates a header underline of specified length
underline = flip replicate "="
