{-# LANGUAGE  OverloadedStrings #-}

module JsonSpec (spec) where

import Test.Hspec -- (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck
import Internal.Api
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.ByteString.Char8 as B (ByteString)
import Network.Wreq
import Network.Wreq.Types (FormValue, renderFormValue)
import Control.Lens ((&), (^.), (^?))
import Data.Either (isRight)
import Json
import Data.Aeson

exampleJournal :: BL.ByteString
exampleJournal = "{\"journal\":{\"userid\":\"1\",\"shortname\":\"root\",\"jtype\":\"1\",\"ctime\":\"1372351074\",\"access\":\"0\"},\"result\":\"0\"}"

spec :: Spec
spec = do
  describe "Journal" $ do
    it "decodes correctly" $ do
      decode exampleJournal `shouldBe` Just Journal {userid = "1", shortname = "root"}
  -- describe "Journal" $ do
  --   it "decodes correctly" $ do
  --     decodeStrict exampleJournal `shouldBe` Just JJournal {journal = Journal {userid = "1", shortname = "root"}}