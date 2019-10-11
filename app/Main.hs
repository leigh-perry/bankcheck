{-# LANGUAGE FlexibleContexts #-}

import qualified Analyser                   as A
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans.Except
import           Data.Semigroup             ((<>))
import           Options.Applicative

main :: IO ()
main = do
  options <- execParser (cmdOptions ? "Bank check operations")
  result <- runExceptT $ run options
  case result of
    Left s  -> putStrLn $ "Error: " <> show s
    Right _ -> return ()

type DateString = String -- 20190111

type WhitelistFilepath = String

type AmountGte = Integer

type AmountLt = Integer

data Command
  = AnalyseTotals [FilePath] (Maybe WhitelistFilepath) (Maybe AmountGte) (Maybe AmountLt)
  | AnalyseSince DateString [FilePath] (Maybe WhitelistFilepath)

data Options =
  Options (Maybe String) Command

run :: (MonadError A.AnalyserError m, MonadIO m) => Options -> m ()
run (Options _ cmd) =
  case cmd of
    AnalyseTotals filepaths whitelistFilepath filterGte filterLt -> A.analyseTotals filepaths whitelistFilepath filters
      where filters = toList filterGte A.txnFilterGteCents <> toList filterLt A.txnFilterLtCents
            toList m f =
              case m of
                Nothing -> []
                Just d  -> [f (d * 100)]
    AnalyseSince startDate filepaths whitelistFilepath -> A.analyseSince startDate filepaths whitelistFilepath

-- approach from https://thoughtbot.com/blog/applicative-options-parsing-in-haskell
cmdOptions :: Parser Options
cmdOptions =
  Options <$>
  optional (strOption (short 'g' <> long "global-option" <> metavar "GLOBALOPTION" <> help "Some option global to all commands")) <*>
  subparser (command "totals" totals <> command "since" since)
  where
    totals =
      (AnalyseTotals <$> filepaths <*> optional whitelist <*>
       optional (option auto (short 'a' <> long "filter-gte" <> metavar "DOLLARS" <> help "Filter txns >= dollar amount")) <*>
       optional (option auto (short 'b' <> long "filter-lt" <> metavar "DOLLARS" <> help "Filter txns < dollar amount"))) ?
      "Analyse statement totals in specified files"
    since = (AnalyseSince <$> sinceDate <*> filepaths <*> optional whitelist) ? "Analyse entries since date"
    filepaths = some (argument str (metavar "SOURCE-FILEPATH"))
    sinceDate = argument str (metavar "YYYYMMDD")
    whitelist = strOption (short 'w' <> long "whitelist" <> metavar "WHITELIST FILE" <> help "File of whitelisted vendors")

(?) :: Parser a -> String -> ParserInfo a
(?) opts desc = info (helper <*> opts) $ progDesc desc
