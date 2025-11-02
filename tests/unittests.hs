import Test.Tasty
import qualified Tests.DocTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Tests.DocTests.tests]

