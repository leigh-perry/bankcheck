module Analyser
  ( run
  ) where

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
run :: [FilePath] -> IO ()
run = print
