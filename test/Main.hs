import qualified Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  test <- testSpec "bankcheck" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ it "dummy" $ True `shouldBe` True
