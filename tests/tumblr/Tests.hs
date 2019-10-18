import           Test.Tasty

import qualified DataTest
import qualified QueryTest
import qualified ResponseTest

main :: IO ()
main = defaultMain $ testGroup "Web.Sleep" tests
  where tests = [ DataTest.tests
                , QueryTest.tests
                , ResponseTest.tests
                ]
