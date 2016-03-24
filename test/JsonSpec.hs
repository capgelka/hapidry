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
import Internal.Json
import Data.Aeson

--{"discuss_count":"0","discuss":[],"comments_count":"0","comments":[],"umail_count":"5","umail":[],"result":"0"}

exampleJournal :: BL.ByteString
exampleJournal = "{\"journal\":{\"userid\":\"1\",\"shortname\":\"root\",\"jtype\":\"1\",\"ctime\":\"1372351074\",\"access\":\"0\"},\"result\":\"0\"}"

exampleNotifications :: BL.ByteString
exampleNotifications = "{\"discuss_count\":\"0\",\"discuss\":[],\"comments_count\":\"0\"\
                        \,\"comments\":[],\"umail_count\":\"5\",\"umail\":[],\"result\":\"0\"}"
-- exampleNotifications = "{\"discuss_count\":\"0\",\"discuss\":[],\"comments_count\"\
-- \:\"0\",\"comments\":[],\"umail_count\":\"5\",\"umail\":{\"320918931\":{\"umailid\":\"320918931\"\
-- \,\"from_userid\":\"3378442\",\"from_username\":\"\\u0447\\u0443\\u0442\\u0435\\u0441\\u0442\"\
-- \,\"dateline\":\"1458086610\",\"message_txt\":\"\\u0442\\u0435\\u0441\\u0442\",\"title\"\
-- \:\"\\u0411\\u0435\\u0437 \\u0442\\u0435\\u043c\\u044b\",\"folder\":\"1\"},\"320918925\":{\"\
-- \umailid\":\"320918925\",\"from_userid\":\"3378442\",\"from_username\":\"\\u0447\\u0443\\u0442\\u\
-- \0435\\u0441\\u0442\",\"dateline\":\"1458086589\",\"message_txt\":\"\\u04\
-- \42\\u0435\\u0441\\u0442\",\"title\":\"\\u0411\\u0435\\u0437 \\u0442\\u0435\\u043c\\u044\
-- \b\",\"folder\":\"\
-- \1\"},\"320918919\":{\"umailid\":\"320918919\",\"from_userid\":\"3378442\",\"from_usernam\
-- \e\":\"\\u0447\\u0443\\u0442\\u0435\\u0441\\u0442\",\"dateline\":\"1458086566\",\"message_tx\
-- \t\":\"\\u0442\\u0435\\u\
-- \0441\\u0442\",\"title\":\"\\u0411\\u0435\\u0437 \\u0442\\u0435\\u043c\\u044b\",\"fold\
-- \er\":\"1\"},\"320\
-- \401836\":{\"umailid\":\"320401836\",\"from_userid\":\"3374983\",\"from_username\":\"haste\
-- \st\",\"dateline\":\"1457052193\",\"message_txt\":\"\\u041f\\u0440\\u0438\\u0432\\u0435\\u044\
-- \2, \\u0442\\u0\
-- \435\\u043f\\u0435\\u0440\\u044c \\u0443 \\u0442\\u0435\\u0431\\u044f \\u04\
-- \34\\u0432\\u0430 \\u0443\\u043c\\u044b\\u043b\\u0430\",\"title\":\"\\u044d\\u0442\\u04\
-- \3e \\u0442\\u0435\\u0441\\u0442\",\"fol\
-- \der\":\"1\"},\"320401824\":{\"umailid\":\"320401824\",\"from_userid\":\"3374983\",\"from_use\
-- \rname\
-- \\":\"hastest\",\"dateline\":\"1457052190\",\"message_txt\":\"\\u041f\\u0440\\u0438\\u043\
-- \2\\u0435\\u0442, \\u0442\\u0435\\u043f\\u0435\\u0440\\u044c \\u0443 \\u0442\\u0435\\u04\
-- \31\\u044f \\u0434\\u0432\\u0430 \\u0443\\u043c\\u044b\\u043b\\u0430\",\"title\":\"\\u04\
-- \4d\\u0442\\u043e \\u0442\\u0435\\u0441\\u0442\",\"folder\":\"1\"}},\"result\":\"0\"}"

disc :: BL.ByteString
disc = "{\"journal_name\": \"1\", \"postid\": \"12\", \"message_txt\":\"TEST\"}"

dlist ::  BL.ByteString
dlist = "[{\"journal_name\": \"1\", \"postid\": \"12\", \"message_txt\":\"TEST\"}]"

spec :: Spec
spec = do
  describe "Internals" $ do
    it "decodes Discussion" $ do
        eitherDecode disc  `shouldBe` (Right (Discussion "1" "12" "TEST"))
    it "decodes empty DiscussionList " $ do
        eitherDecode "[]"  `shouldBe` (Right (DiscussionList []))
    -- it "decodes DiscussionList" $ do
        -- let c = eitherDecode dlist
        -- eitherDecode exampleJournal `shouldBe` Right Journal {userid = "1", shortname = "root"}
        --eitherDecode dlist `shouldBe` Right []
    it "decodes DiscussionList" $ do
        (eitherDecode dlist) `shouldBe` (Right (DiscussionList [Discussion "1" "root" "Xeeee"]))
  describe "Journal" $ do
    it "decodes correctly" $ do
        eitherDecode exampleJournal `shouldBe` Right Journal {userid = "1", shortname = "root"}
  describe "Notifications" $ do
    it "decodes correctly" $ do
        eitherDecode exampleNotifications `shouldBe` Right Notifications { umailCount = 0,
                                                                    commentsCount = 0,
                                                                    discussCount = 0,
                                                                    comments = CommentList [],
                                                                    umails = UmailList [],
                                                                    discussions = DiscussionList []}