module Spec where

import           Control.Monad
import qualified Data.Text as T
import           Lib
import           Text.Printf
import           Test.Hspec

readSamples :: IO [(T.Text,T.Text)]
readSamples = do
    -- mapM getSample =<< T.lines <$> T.pack <$> readFile "test/sample.txt"
    mapM getSample . (T.lines . T.pack)
                =<< readFile "sample.txt"
      where
        getSample line =
          case (T.splitOn . T.pack) "," line of
            [input, output] -> pure (input,output)
            _ -> fail "invalid sample"
  
main :: IO ()
main = hspec $ do
  sample <- runIO readSamples
  forM_ sample (\(input, output) ->
    it (printf "plural of '%s'is '%s'" input output) $
      pluralize input `shouldBe` output)