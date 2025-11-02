import Test.Tasty
import qualified Tests.DocTests
import qualified Tests.BufferTests
import qualified Tests.Utils.ListUtilsTests
import qualified Tests.Renderer.ViewPortTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [Tests.DocTests.tests, Tests.BufferTests.tests, Tests.Utils.ListUtilsTests.tests, Tests.Renderer.ViewPortTests.tests]

