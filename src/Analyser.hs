{-# LANGUAGE OverloadedStrings #-}

module Analyser
  ( analyse
  ) where

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import qualified Data.ByteString.Lazy as BL
import Data.Csv ((.:))
import qualified Data.Csv as Csv
import Data.Foldable (toList, traverse_)
import Data.Functor (void)
import Data.Text
import qualified Data.Vector as V
import Text.Parsec (ParseError)
import Text.Parsec.Char as PC
import qualified Text.ParserCombinators.Parsec as P

--import Debug.Trace
{-
  Specification
    . read specified csv files, eg ~/Dropbox/LPSM/personal/visa/20190701.csv
    . dedup csv lines
    . parse: Effective Date,Entered Date,Transaction Description,Amount,Balance
    . sort by txn date
    . group by transaction description
    . compare with whitelist
    . show remaining groups
-}
analyse :: [FilePath] -> ExceptT AnalyserError IO ()
analyse filepaths =
  void $ do
    files <- traverse parseCsvFile filepaths -- traverse gives IO [[Entry]]
    let entries = join files
    traverse_ parseDescription entries

parseDescription :: CsvEntry -> ExceptT AnalyserError IO ()
parseDescription v = do
  detail <-
    except $
    case P.parse descriptionParser "Description" $ unpack (cDescription v) of
      Left e -> Left $ ParseDescriptionError e
      Right d -> Right d
  liftIO $ print $ Entry (cEffectiveDate v) (cEnteredDate v) detail (cAmount v) (cBalance v)

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
  except $
    case d of
      Left e -> Left $ ParseCsvError e
      Right (_, v) -> Right $ toList v

fixedLengthStr :: Int -> P.Parser String
fixedLengthStr n = P.count n anyChar

-- "VISA"[" Refund"]"-"{1234567890123456789012345}{"varlength"}"(Ref."{123456789012}")"
descriptionParser :: P.Parser Detail
descriptionParser = do
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
      (strip (pack vendor))
      (pack details)
      (pack refno)
