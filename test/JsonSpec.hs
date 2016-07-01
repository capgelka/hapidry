{-# LANGUAGE  OverloadedStrings, DuplicateRecordFields #-}

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
import qualified Internal.Json as IJ
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
post = "{\"dateline_date\": \"100\", \"postid\": \"12\", \"message_html\": \"TEST\",\
\ \"title\": \"test\", \"comments_count_data\": \"12\", \"shortname\": \"user\",\
\ \"journal_name\": \"user_journal\"}"

posts :: BL.ByteString
posts = "{\"posts\": {\"12\":{\"dateline_date\": \"100\", \"postid\": \"13\", \"message_html\": \"TEST\",\
\ \"title\": \"test\", \"comments_count_data\": \"12\", \"shortname\": \"user\",\
\ \"journal_name\": \"user_journal\"},\
\ \"13\": {\"dateline_date\": \"100\", \"postid\": \"13\", \"message_html\": \"TEST\",\
\ \"title\": \"test\", \"comments_count_data\": \"12\", \"shortname\": \"user\",\
\ \"journal_name\": \"user_journal\"}}}" 

uMessage :: BL.ByteString
uMessage = "{\"dateline\": \"100\", \"umailid\": \"12\", \"message_html\": \"TEST\",\
\ \"title\": \"test\", \"from_username\": \"someone\" }"

messageList :: BL.ByteString
messageList = "{\"umail\": {\"12\": {\"dateline\": \"100\", \"umailid\": \"12\", \"message_html\": \"TEST\",\
\ \"title\": \"test\", \"from_username\": \"someone\" },\
\ \"13\": {\"dateline\": \"100\", \"umailid\": \"13\", \"message_html\": \"TEST\",\
\ \"title\": \"test\", \"from_username\": \"someone\" }}}"



spec :: Spec
spec = do
  describe "Internals" $ do
    it "decodes Discussion" $ do
        eitherDecode disc  `shouldBe` (Right (IJ.Discussion "1" "12" "TEST"))
    it "decodes empty DiscussionList " $ do
        eitherDecode "[]"  `shouldBe` (Right (IJ.DiscussionList []))
    it "decodes DiscussionList " $ do
        eitherDecode discList `shouldBe` Right (IJ.DiscussionList 
                                                    [IJ.Discussion "1" "2423535" "TEST",
                                                     IJ.Discussion "1" "1233" "TEST"])
    it "decodes umail" $ do
        eitherDecode umail  `shouldBe` Right (IJ.Umail "someone" "test" "TEST")
    it "decodes empty umailList " $ do
        eitherDecode "[]"  `shouldBe` (Right (IJ.UmailList []))
    it "decodes umailList " $ do
        eitherDecode umailList `shouldBe` Right (IJ.UmailList [IJ.Umail "petya" "Re ..." "TEST",
                                                               IJ.Umail "vova" "Hi!" "TEST"])

    it "decodes Comment" $ do
        eitherDecode disc  `shouldBe` (Right (IJ.Comment "12" "TEST"))
    it "decodes empty CommentList " $ do
        eitherDecode "[]"  `shouldBe` (Right (IJ.CommentList []))
    it "decodes CommentList " $ do
        eitherDecode discList `shouldBe` Right (IJ.CommentList [IJ.Comment "2423535" "TEST",
                                                                IJ.Comment "1233" "TEST"])

    it "decodes Post" $ do 
        eitherDecode post `shouldBe` (Right $ IJ.Post "12" 
                                                     "100"
                                                     "12" 
                                                     "test" 
                                                     "TEST" 
                                                     "user"
                                                     (Just "user_journal"))

    it "decodes PostList" $ do 
        eitherDecode posts `shouldBe` (Right $ IJ.PostList $
                                                  [IJ.Post "13" 
                                                     "100"
                                                     "12" 
                                                     "test" 
                                                     "TEST" 
                                                     "user"
                                                     (Just "user_journal"),
                                                   IJ.Post "12" 
                                                     "100"
                                                     "12" 
                                                     "test" 
                                                     "TEST" 
                                                     "user"
                                                     (Just "user_journal")])

    it "decodes Umail message" $ do 
        eitherDecode uMessage `shouldBe` (Right $ IJ.UmailMessage "12" 
                                                               "100"
                                                               "test" 
                                                               "TEST" 
                                                               "someone")
    it "decodes Umail messages" $ do 
        eitherDecode messageList `shouldBe` (Right $ IJ.MessageList $
                                                  [IJ.UmailMessage "13" 
                                                                   "100"
                                                                   "test" 
                                                                   "TEST" 
                                                                   "someone",
                                                   IJ.UmailMessage "12" 
                                                                   "100"
                                                                   "test" 
                                                                   "TEST" 
                                                                   "someone"])
    -- it "decodes Post2" $ do 
    --     eitherDecode "{\"message_html\": \"TEST\"}" `shouldBe` Right (Post "12" "100" "12" "test" "TEST")

   
  describe "Journal" $ do
    it "decodes correctly" $ do
        eitherDecode exampleJournal `shouldBe` Right Journal {userid = "1", shortname = "root"}
  describe "Notifications" $ do
    it "decodes correctly \"empty\" notifications" $ do
        eitherDecode emptyNotifications `shouldBe` Right Notifications { umailCount = 5,
                                                                    commentsCount = 0,
                                                                    discussCount = 0,
                                                                    comments = IJ.CommentList [],
                                                                    umails = IJ.UmailList [],
                                                                    discussions = IJ.DiscussionList []}
    it "decodes correctly nonempty notifications" $ do
        eitherDecode exampleNotifications
        `shouldBe` 
        Right Notifications { umailCount = 5,
                              commentsCount = 0,
                              discussCount = 0,
                              comments = IJ.CommentList [IJ.Comment "2423535" "TEST",
                                                         IJ.Comment "1233" "TEST"],
                              umails = IJ.UmailList [],
                              discussions = IJ.DiscussionList []}