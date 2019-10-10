{-# LANGUAGE OverloadedStrings #-}

module Analyser
  ( analyseTotals
  , TxnFilter
  , txnFilterNone
  , txnFilterGteCents
  , txnFilterLtCents
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString.Lazy as BL
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import Data.Foldable (toList, traverse_)
import Data.List (sortOn)
import qualified Data.List.Utils as U (uniq)
import Data.Map (assocs)
import qualified Data.Map as M
import Data.Monoid
import Data.Text hiding (all, filter, groupBy)
import qualified Data.Vector as V
import Text.Parsec (ParseError, (<|>))
import Text.Parsec.Char as PC
import qualified Text.ParserCombinators.Parsec as P

data TxnFilter
  = TxnFilterNone
  | TxnFilterGteCents Integer
  | TxnFilterLtCents Integer

txnFilterNone :: TxnFilter
txnFilterNone = TxnFilterNone

txnFilterGteCents :: Integer -> TxnFilter
txnFilterGteCents = TxnFilterGteCents

txnFilterLtCents :: Integer -> TxnFilter
txnFilterLtCents = TxnFilterLtCents

analyseTotals :: [FilePath] -> [TxnFilter] -> ExceptT AnalyserError IO ()
analyseTotals filepaths txnFilter = do
  files <- traverse parseCsvFile filepaths
  let entries = sortOn cEnteredDate $ U.uniq $ join files
  es <- traverse parseDescription entries
  let byVendor = groupBy $ keyByVendor <$> es
  --let vendors = M.keys byVendor
  let totalsByVendor = foldMap (\e -> (Sum 1, Sum (cents $ -(eAmount e)))) <$> byVendor
  let entrySet = assocs totalsByVendor
  let filtered = filter (passesTxnFilters txnFilter . snd) entrySet
  liftIO $ traverse_ putStrLn $ formatTotal <$> filtered

parseDescription :: CsvEntry -> ExceptT AnalyserError IO Entry
parseDescription v = do
  detail <- except $ first ParseDescriptionError $ P.parse detailParser "Description" $ unpack (cDescription v)
  return $ Entry (cEffectiveDate v) (cEnteredDate v) detail (cAmount v) (cBalance v)

groupBy :: Ord k => [(k, a)] -> M.Map k [a]
groupBy kvs = M.fromListWith (<>) [(k, [v]) | (k, v) <- kvs]

keyByVendor :: Entry -> (Text, Entry)
keyByVendor e =
  case e of
    Entry _ _ (Txn _ vendor _ _) _ _ -> (vendor, e)
    Entry _ _ (General _) _ _ -> ("(none)", e)

type Agg = (Sum Integer, Sum Integer)

passesTxnFilters :: [TxnFilter] -> Agg -> Bool
passesTxnFilters fs a = all (passesTxnFilter a) fs

passesTxnFilter :: Agg -> TxnFilter -> Bool
passesTxnFilter _ TxnFilterNone = True
passesTxnFilter a (TxnFilterGteCents c) = getSum (snd a) >= c
passesTxnFilter a (TxnFilterLtCents c) = getSum (snd a) < c

formatTotal :: (Text, Agg) -> String
formatTotal (v, (c, t)) = unpack v <> ": " <> show (getSum c) <> " @ $" <> show (dollars $ getSum t)

----
-- Effective Date,Entered Date,Transaction Description,Amount,Balance
-- ,20190701,"VISA-TRANSPORTFORNSW OPAL     CHIPPENDALE  AU#0428976(Ref.070100303466)",-20.00,-11846.85
data CsvEntry =
  CsvEntry
    { cEffectiveDate :: !Text
    , cEnteredDate :: !Text
    , cDescription :: !Text
    , cAmount :: !Double
    , cBalance :: !Double
    }
  deriving (Show, Eq)

data TxnType
  = Purchase
  | Refund
  deriving (Show, Eq)

data Detail
  = General
      { gDescription :: !Text
      }
  | Txn
      { eType :: !TxnType
      , eVendor :: !Text
      , eDetails :: !Text
      , eRef :: !Text
      }
  deriving (Show, Eq)

data Entry =
  Entry
    { eEffectiveDate :: !Text
    , eEnteredDate :: !Text
    , eDetail :: !Detail
    , eAmount :: !Double
    , eBalance :: !Double
    }
  deriving (Show, Eq)

cents :: Double -> Integer
cents = round . (* 100.0)

dollars :: Integer -> Double
dollars = (/ 100.0) . fromIntegral

data AnalyserError
  = ParseCsvError String
  | ParseDescriptionError ParseError
  deriving (Show, Eq)

instance Csv.FromNamedRecord CsvEntry where
  parseNamedRecord r =
    CsvEntry <$> r .: "Effective Date" <*> r .: "Entered Date" <*> r .: "Transaction Description" <*> r .: "Amount" <*>
    r .: "Balance"

parseCsvFile :: String -> ExceptT AnalyserError IO [CsvEntry]
parseCsvFile filepath = do
  csvData <- liftIO $ BL.readFile filepath
  let d = Csv.decodeByName csvData :: Either String (Csv.Header, V.Vector CsvEntry)
  except $ bimap ParseCsvError (toList . snd) d

fixedLengthStr :: Int -> P.Parser String
fixedLengthStr n = P.count n anyChar

detailParser :: P.Parser Detail
detailParser = txnParser <|> generalParser

-- "VISA"[" Refund"]"-"{1234567890123456789012345}{"varlength"}"(Ref."{123456789012}")"
txnParser :: P.Parser Detail
txnParser = do
  refundInd <- string "VISA" *> P.optionMaybe (string " Refund") <* char '-'
  vendor <- fixedLengthStr 25
  let sref = string "(Ref."
  details <- P.manyTill anyChar $ P.try $ P.lookAhead sref
  refno <- sref *> fixedLengthStr 12 <* char ')'
  return $
    Txn
      (case refundInd of
         Just _ -> Refund
         Nothing -> Purchase)
      (strip $ pack vendor)
      (pack details)
      (pack refno)

-- "Cash Advances Interest: $1.98"
generalParser :: P.Parser Detail
generalParser = do
  details <- P.many anyChar
  return $ General (pack details)
