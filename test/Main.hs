{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List      as List
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

main :: IO ()
main = do
  result <- tests
  putStrLn $
    if result
      then "All tests passed"
      else "At least one test failed"

tests :: IO Bool
tests = checkParallel $ Group "Some group" [("List reversing", testReverse), ("List reversing too", testReverse2)]

genIntList :: Gen [Int]
genIntList =
  let listLength = Range.linear 0 100
   in Gen.list listLength Gen.enumBounded

testReverse :: Property
testReverse =
  property $ do
    xs <- forAll genIntList
    List.reverse (List.reverse xs) === xs

testReverse2 :: Property
testReverse2 =
  withTests 1000 . property $ do
    xs <- forAll genIntList
    List.reverse (List.reverse xs) === xs
