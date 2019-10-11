{-# LANGUAGE OverloadedStrings #-}

module Analyser
  ( analyseTotals
  , analyseSince
  , AnalyserError
  , TxnFilter
  , txnFilterNone
  , txnFilterGteCents
  , txnFilterLtCents
  ) where

import           Control.Monad                 (join)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Trans.Except    (ExceptT, except)
import           Data.Bifunctor                (bimap, first)
import qualified Data.ByteString.Lazy          as BL
import           Data.Csv                      ((.:))
import qualified Data.Csv                      as Csv
import           Data.Foldable                 (toList, traverse_)
import           Data.List                     (sortOn)
import qualified Data.List.Utils               as U (uniq)
import           Data.Map                      (assocs)
import qualified Data.Map                      as M
import           Data.Monoid
import           Data.Text                     as T hiding (all, any, break,
                                                     filter, groupBy)
import qualified Data.Vector                   as V
import           Text.Parsec                   (ParseError, (<|>))
import           Text.Parsec.Char              as PC
import qualified Text.ParserCombinators.Parsec as P
import           Text.Printf                   (printf)
import           Text.Regex.Posix              ((=~))

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

analyseTotals :: MonadIO m => [FilePath] -> Maybe String -> [TxnFilter] -> ExceptT AnalyserError m ()
analyseTotals filepaths whitelistFilepath txnFilter = do
  whitelisted <- ingest filepaths whitelistFilepath
  let byVendor = groupBy $ keyByVendor <$> whitelisted
  -- aggregate number of txns and total
  let totalsByVendor = foldMap (\e -> (Sum 1, Sum (cents $ -(eAmount e)))) <$> byVendor
  let filtered = filter (passesTxnFilters txnFilter . snd) $ assocs totalsByVendor
  liftIO $ traverse_ putStrLn $ formatTotal <$> filtered

analyseSince :: MonadIO m => String -> [FilePath] -> Maybe String -> ExceptT AnalyserError m ()
analyseSince startDate filepaths whitelistFilepath = do
  whitelisted <- ingest filepaths whitelistFilepath
  let (before, after) = break (\e -> unpack (eEnteredDate e) >= startDate) whitelisted
  -- find entries in `after` that have never been seen `before`
  let beforebyVendor = groupBy $ keyByVendor <$> before
  let previouslyUnseen =
        filter
          (\e ->
             case eDetail e of
               Txn _ v _ _ -> not (M.member v beforebyVendor)
               General _   -> True)
          after
  liftIO $ traverse_ putStrLn $ formatEntry <$> previouslyUnseen

----
ingest :: MonadIO m => [FilePath] -> Maybe String -> ExceptT AnalyserError m [Entry]
ingest filepaths whitelistFilepath = do
  files <- traverse parseEntryFile filepaths
  whitelist <-
    case whitelistFilepath of
      Just wl -> parseWhitelist wl
      Nothing -> return []
  let entries = sortOn cEnteredDate $ U.uniq $ join files
  es <- traverse parseDescription entries
  return $ filterWhitelist whitelist es

----
parseDescription :: MonadIO m => CsvEntry -> ExceptT AnalyserError m Entry
parseDescription v = do
  detail <- except $ first ParseDescriptionError $ P.parse detailParser "Description" $ unpack (cDescription v)
  return $ Entry (cEffectiveDate v) (cEnteredDate v) detail (cAmount v) (cBalance v)

-- remove any records where entry in whitelist and amount is less than limit
filterWhitelist :: [WhitelistEntry] -> [Entry] -> [Entry]
filterWhitelist wls =
  filter
    (\e ->
       case eDetail e of
         Txn _ v _ _ -> not (any (shouldHide e v) wls)
         General _   -> True)
  where
    shouldHide e v w = unpack v =~ unpack (wVendorRegex w) && -(eAmount e) < wTxnLimit w

groupBy :: Ord k => [(k, a)] -> M.Map k [a]
groupBy kvs = M.fromListWith (<>) [(k, [v]) | (k, v) <- kvs]

keyByVendor :: Entry -> (Text, Entry)
keyByVendor e =
  case e of
    Entry _ _ (Txn _ vendor _ _) _ _ -> (vendor, e)
    Entry _ _ (General _) _ _        -> ("(none)", e)

type Agg = (Sum Integer, Sum Integer)

passesTxnFilters :: [TxnFilter] -> Agg -> Bool
passesTxnFilters fs a = all (passesTxnFilter a) fs

passesTxnFilter :: Agg -> TxnFilter -> Bool
passesTxnFilter _ TxnFilterNone         = True
passesTxnFilter a (TxnFilterGteCents c) = getSum (snd a) >= c
passesTxnFilter a (TxnFilterLtCents c)  = getSum (snd a) < c

formatTotal :: (Text, Agg) -> String
formatTotal (v, (c, t)) = unpack v <> ": " <> show (getSum c) <> " @ $" <> show (dollars $ getSum t)

-- Entry {eEffectiveDate = "", eEnteredDate = "20191003", eDetail = Txn {eType = Purchase, eVendor = "NAZARI", eDetails = "GRANADA      ESFRGN AMT-55.000000#0457223", eRef = "100300976257"}, eAmount = -92.59, eBalance = -11075.28}
formatEntry :: Entry -> String
formatEntry e =
  case e of
    Entry _ date (Txn ttype vendor details _) amt _ ->
      printf "%s %-25s $%8.2f  %-42s (%s)" date vendor (-amt) details (show ttype)
    Entry _ date (General g) _ _ -> printf "%s                                      %s" date g

----
-- Effective Date,Entered Date,Transaction Description,Amount,Balance
-- ,20190701,"VISA-TRANSPORTFORNSW OPAL     CHIPPENDALE  AU#0428976(Ref.070100303466)",-20.00,-11846.85
data CsvEntry =
  CsvEntry
    { cEffectiveDate :: !Text
    , cEnteredDate   :: !Text
    , cDescription   :: !Text
    , cAmount        :: !Double
    , cBalance       :: !Double
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
      { eType    :: !TxnType
      , eVendor  :: !Text
      , eDetails :: !Text
      , eRef     :: !Text
      }
  deriving (Show, Eq)

data Entry =
  Entry
    { eEffectiveDate :: !Text
    , eEnteredDate   :: !Text
    , eDetail        :: !Detail
    , eAmount        :: !Double
    , eBalance       :: !Double
    }
  deriving (Show, Eq)

data WhitelistEntry =
  WhitelistEntry
    { wVendorRegex :: !Text
    , wTxnLimit    :: !Double
    }
  deriving (Show, Eq)

cents :: Double -> Integer
cents = round . (* 100.0)

dollars :: Integer -> Double
dollars = (/ 100.0) . fromIntegral

data AnalyserError
  = ParseEntryError String
  | ParseDescriptionError ParseError
  | ParseWhitelistError String
  deriving (Show, Eq)

instance Csv.FromNamedRecord CsvEntry where
  parseNamedRecord r =
    CsvEntry <$> r .: "Effective Date" <*> r .: "Entered Date" <*> r .: "Transaction Description" <*> r .: "Amount" <*>
    r .: "Balance"

parseEntryFile :: MonadIO m => String -> ExceptT AnalyserError m [CsvEntry]
parseEntryFile filepath = do
  csvData <- liftIO $ BL.readFile filepath
  let d = Csv.decodeByName csvData :: Either String (Csv.Header, V.Vector CsvEntry)
  except $ bimap ParseEntryError (toList . snd) d

instance Csv.FromNamedRecord WhitelistEntry where
  parseNamedRecord r = WhitelistEntry <$> r .: "Vendor Regex" <*> r .: "Per Txn Limit"

parseWhitelist :: MonadIO m => String -> ExceptT AnalyserError m [WhitelistEntry]
parseWhitelist filepath = do
  csvData <- liftIO $ BL.readFile filepath
  let d = Csv.decodeByName csvData :: Either String (Csv.Header, V.Vector WhitelistEntry)
  except $ bimap ParseWhitelistError (toList . snd) d

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
         Just _  -> Refund
         Nothing -> Purchase)
      (strip $ pack vendor)
      (pack details)
      (pack refno)

-- "Cash Advances Interest: $1.98"
generalParser :: P.Parser Detail
generalParser = do
  details <- P.many anyChar
  return $ General (pack details)
