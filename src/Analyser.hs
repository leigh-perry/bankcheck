{-# LANGUAGE OverloadedStrings #-}

module Analyser
  ( analyse
  ) where

import qualified Data.ByteString.Lazy as BL
import           Data.Csv             ((.:))
import qualified Data.Csv             as Csv
import           Data.Foldable        (traverse_)
import           Data.Functor         (void)
import           Data.Text
import qualified Data.Vector          as V

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
analyse :: [FilePath] -> IO ()
analyse filepaths =
  void $ do
    files <- traverse analyseFile filepaths -- IO [V.Vector Entry]
    traverse dump files -- IO [()]

dump :: V.Vector Entry -> IO ()
dump v = traverse_ print v

----
-- Effective Date,Entered Date,Transaction Description,Amount,Balance
-- ,20190701,"VISA-TRANSPORTFORNSW OPAL     CHIPPENDALE  AU#0428976(Ref.070100303466)",-20.00,-11846.85
data Entry =
  Entry
    { eEffectiveDate :: !Text
    , eEnteredDate   :: !Text
    , eDescription   :: !Text
    , eAmount        :: !Double
    , eBalance       :: !Double
    }
  deriving (Show, Eq)

instance Csv.FromNamedRecord Entry where
  parseNamedRecord r =
    Entry <$> r .: "Effective Date" <*> r .: "Entered Date" <*> r .: "Transaction Description" <*> r .: "Amount" <*>
    r .: "Balance"

analyseFile :: String -> IO (V.Vector Entry)
analyseFile filepath = do
  csvData <- BL.readFile filepath
  let d = Csv.decodeByName csvData :: Either String (Csv.Header, V.Vector Entry)
  return $
    case d of
      Left err     -> error err -- TODO ExceptT
      Right (_, v) -> v
