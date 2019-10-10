{-# LANGUAGE LambdaCase #-}

import qualified Analyser                   as A
import           Control.Monad.Trans.Except
import           Data.Semigroup             ((<>))
import           Options.Applicative

main :: IO ()
main = run =<< execParser (parseOptions ? "Bank check operations")

type GlobalOption = String

type BuildId = String

data Command
  = AnalyseTotals [FilePath] (Maybe Integer) (Maybe Integer) (Maybe String)
  | Dummy BuildId

-- approach from https://thoughtbot.com/blog/applicative-options-parsing-in-haskell
-- bankcheck --global-option analyse filename ... filename
-- bankcheck --global-option dummy <build-id>
data Options =
  Options (Maybe GlobalOption) Command

run :: Options -> IO ()
run (Options globalOption cmd) =
  case cmd of
    AnalyseTotals filepaths filterGte filterLt whitelistFilepath ->
      runExceptT (A.analyseTotals filepaths filters whitelistFilepath) >>=
      (\case
         Left s -> putStrLn $ "Error: " <> show s
         Right _ -> return ())
      where filters = toList filterGte A.txnFilterGteCents <> toList filterLt A.txnFilterLtCents
            toList m f =
              case m of
                Nothing -> []
                Just d  -> [f (d * 100)]
    Dummy buildId -> putStrLn $ show globalOption <> " : " <> buildId

parseOptions :: Parser Options
parseOptions =
  Options <$>
  optional (strOption (short 'g' <> long "global-option" <> metavar "GLOBALOPTION" <> help "Some option global to all commands")) <*>
  subparser (command "totals" analyseTotals <> command "dummy" dummy)
  where
    analyseTotals =
      (AnalyseTotals <$> some (argument str (metavar "SOURCE-FILEPATH")) <*>
       optional
         (option auto (short 'a' <> long "filter-gte" <> metavar "DOLLARS" <> help "Filter txns >= specified dollar amount")) <*>
       optional (option auto (short 'b' <> long "filter-lt" <> metavar "DOLLARS" <> help "Filter txns < specified dollar amount")) <*>
       optional (strOption (short 'w' <> long "whitelist" <> metavar "WHITELIST FILE" <> help "File of whitelisted vendors"))) ?
      "Analyse statement totals in specified files"
    dummy = (Dummy <$> argument str (metavar "BUILD-ID")) ? "Dummy command taking a build id"

(?) :: Parser a -> String -> ParserInfo a
(?) opts desc = info (helper <*> opts) $ progDesc desc
