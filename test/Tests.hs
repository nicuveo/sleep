import           Test.Tasty

import qualified Web.Sleep.Tumblr.DataTest
import qualified Web.Sleep.Tumblr.QueryTest
import qualified Web.Sleep.Tumblr.ResponseTest

main :: IO ()
main = defaultMain $ testGroup "Web.Sleep" tests
  where tests = [ Web.Sleep.Tumblr.DataTest.tests
                , Web.Sleep.Tumblr.QueryTest.tests
                , Web.Sleep.Tumblr.ResponseTest.tests
                ]
