{-# LANGUAGE  OverloadedStrings #-}

module JsonSpec (spec) where

import Test.Hspec -- (Spec, describe, it, shouldSatisfy, shouldBe)
import Test.QuickCheck
import Internal.Api
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)
import qualified Data.ByteString.Char8 as B (ByteString)
import Network.Wreq hiding (post)
import Network.Wreq.Types (FormValue, renderFormValue)
import Control.Lens ((&), (^.), (^?))
import Data.Either (isRight)
import Json
import Internal.Json
import Data.Aeson


exampleJournal :: BL.ByteString
exampleJournal = "{\"journal\":{\"userid\":\"1\",\"shortname\":\"root\",\"jtype\":\"1\",\"ctime\":\"1372351074\",\"access\":\"0\"},\"result\":\"0\"}"

emptyNotifications :: BL.ByteString
emptyNotifications = "{\"discuss_count\":\"0\",\"discuss\":[],\"comments_count\":\"0\"\
                        \,\"comments\":[],\"umail_count\":\"5\",\"umail\":[],\"result\":\"0\"}"


exampleNotifications :: BL.ByteString
exampleNotifications = "{\"discuss_count\":\"0\",\"discuss\":[],\"comments_count\":\"0\"\
                        \,\"comments\":{\"1233\" : {\"postid\": \"12\", \"message_txt\":\"TEST\"}\
                        \, \"2423535\" : {\"postid\": \"12\", \"message_txt\":\"TEST\"}}\
                        \,\"umail_count\":\"5\",\"umail\":[],\"result\":\"0\"}"

comment :: BL.ByteString
comment = "{\"postid\": \"12\", \"message_txt\":\"TEST\"}"

commentList :: BL.ByteString
commentList = "{\"1233\" : {\"postid\": \"12\", \"message_txt\":\"TEST\"}\
\, \"2423535\" : {\"postid\": \"12\", \"message_txt\":\"TEST\"}}"

umail :: BL.ByteString
umail = "{\"from_username\": \"someone\", \"title\": \"test\", \"message_txt\":\"TEST\"}"

umailList :: BL.ByteString
umailList = "{\"1233\" : {\"from_username\": \"vova\", \"title\": \"Hi!\", \"message_txt\":\"TEST\"}\
\, \"2423535\" : {\"from_username\": \"petya\", \"title\": \"Re ...\", \"message_txt\":\"TEST\"}}"

disc :: BL.ByteString
disc = "{\"journal_name\": \"1\", \"postid\": \"12\", \"message_txt\":\"TEST\"}"


discList :: BL.ByteString
discList = "{\"1233\" : {\"journal_name\": \"1\", \"postid\": \"12\", \"message_txt\":\"TEST\"}\
\, \"2423535\" : {\"journal_name\": \"1\", \"postid\": \"12\", \"message_txt\":\"TEST\"}}"


post :: BL.ByteString
post = "{\"dateline_date\": \"100\", \"postid\": \"12\", \"message_html\":\"TEST\",\
\, \"title\": \"test\", \"comments_count_data\": \"12\"}"

spec :: Spec
spec = do
  describe "Internals" $ do
    it "decodes Discussion" $ do
        eitherDecode disc  `shouldBe` (Right (Discussion "1" "12" "TEST"))
    it "decodes empty DiscussionList " $ do
        eitherDecode "[]"  `shouldBe` (Right (DiscussionList []))
    it "decodes DiscussionList " $ do
        eitherDecode discList `shouldBe` Right (DiscussionList [Discussion "1" "2423535" "TEST",
                                                                Discussion "1" "1233" "TEST"])
    it "decodes umail" $ do
        eitherDecode umail  `shouldBe` Right (Umail "someone" "test" "TEST")
    it "decodes empty umailList " $ do
        eitherDecode "[]"  `shouldBe` (Right (UmailList []))
    it "decodes umailList " $ do
        eitherDecode umailList `shouldBe` Right (UmailList [Umail "petya" "Re ..." "TEST",
                                                            Umail "vova" "Hi!" "TEST"])

    it "decodes Comment" $ do
        eitherDecode disc  `shouldBe` (Right (Comment "12" "TEST"))
    it "decodes empty CommentList " $ do
        eitherDecode "[]"  `shouldBe` (Right (CommentList []))
    it "decodes CommentList " $ do
        eitherDecode discList `shouldBe` Right (CommentList [Comment "2423535" "TEST",
                                                             Comment "1233" "TEST"])

    it "decodes Post" $ do 
        eitherDecode post `shouldBe` Right (Post "12" "100" "12" "test" "TEST")

   
  describe "Journal" $ do
    it "decodes correctly" $ do
        eitherDecode exampleJournal `shouldBe` Right Journal {userid = "1", shortname = "root"}
  describe "Notifications" $ do
    it "decodes correctly \"empty\" notifications" $ do
        eitherDecode emptyNotifications `shouldBe` Right Notifications { umailCount = 5,
                                                                    commentsCount = 0,
                                                                    discussCount = 0,
                                                                    comments = CommentList [],
                                                                    umails = UmailList [],
                                                                    discussions = DiscussionList []}
    it "decodes correctly nonempty notifications" $ do
        eitherDecode exampleNotifications
        `shouldBe` 
        Right Notifications { umailCount = 5,
                              commentsCount = 0,
                              discussCount = 0,
                              comments = CommentList [Comment "2423535" "TEST",
                                                      Comment "1233" "TEST"],
                              umails = UmailList [],
                              discussions = DiscussionList []}