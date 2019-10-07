import           Data.Semigroup      ((<>))
import           Options.Applicative

type GlobalOption = String

type BuildId = String

data Command
  = Analyse [FilePath]
  | Dummy BuildId

-- approach from https://thoughtbot.com/blog/applicative-options-parsing-in-haskell
-- bankcheck --global-option analyse filename ... filename
-- bankcheck --global-option dummy <build-id>
data Options =
  Options GlobalOption Command

main :: IO ()
main = run =<< execParser (parseOptions ? "Bank check operations")

run :: Options -> IO ()
run (Options globalOption cmd) =
  case cmd of
    Analyse filePath -> putStrLn $ globalOption <> " : " <> show filePath
    Dummy buildId    -> putStrLn $ globalOption <> " : " <> buildId

parseOptions :: Parser Options
parseOptions =
  Options <$> strOption (short 'g' <> long "global-option" <> metavar "GLOBALOPTION" <> help "Some option global to all commands") <*>
  subparser
    (command "analyse" ((Analyse <$> some (argument str (metavar "SOURCE-FILEPATH"))) ? "Analyse statements in specified files") <>
     command "dummy" ((Dummy <$> argument str (metavar "BUILD-ID")) ? "Dummy command taking a build id"))

(?) :: Parser a -> String -> ParserInfo a
(?) opts desc = info (helper <*> opts) $ progDesc desc
