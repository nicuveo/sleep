import           Test.Tasty

import           Web.Sleep.Tumblr.QueryTest    as T1
import           Web.Sleep.Tumblr.ResponseTest as T2

main :: IO ()
main = defaultMain $ testGroup "Web.Sleep" [ T1.tests
                                           , T2.tests
                                           ]
