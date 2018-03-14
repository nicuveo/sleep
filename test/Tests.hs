import           Test.Tasty

import           Web.Sleep.Tumblr.ResponseTest as T1

main :: IO ()
main = defaultMain $ T1.tests
