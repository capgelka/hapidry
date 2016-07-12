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

-- long :: BL.ByteString
-- long = "{\"result\":\"0\",\"posts\":{\"209723035\":{\"postid\":\"209723035\",\"dateline_date\":\"1468019565\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"test\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1468019565\",\"message_html\":\"test\",\"title\":\"\",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"209651554\":{\"postid\":\"209651554\",\"dateline_date\":\"1467414059\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"tst\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1467414059\",\"message_html\":\"tst\",\"title\":\"\",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"209651526\":{\"postid\":\"209651526\",\"dateline_date\":\"1467413837\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"tst\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1467413837\",\"message_html\":\"tst\",\"title\":\"\",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"209651497\":{\"postid\":\"209651497\",\"dateline_date\":\"1467413793\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"\209\130\208\181\209\129\209\130\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1467413793\",\"message_html\":\"\209\130\208\181\209\129\209\130\",\"title\":\"\",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"208892565\":{\"postid\":\"208892565\",\"dateline_date\":\"1461510292\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"\208\176 \209\141\209\130\208\190 \208\189\208\184\208\186\209\130\208\190 \208\189\208\181 \208\178\208\184\208\180\208\184\209\130\",\"access\":\"4\",\"close_access_mode\":\"4\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1461510292\",\"message_html\":\"\208\176 \209\141\209\130\208\190 \208\189\208\184\208\186\209\130\208\190 \208\189\208\181 \208\178\208\184\208\180\208\184\209\130\",\"title\":\"\",\"ico\":\"<img src=\\\"http:\\/\\/static.diary.ru\\/images\\/offlock2.gif\\\" class=\\\"opac6\\\" align=\\\"absmiddle\\\" alt=\\\"lock\\\" title=\\\"\208\151\208\176\208\191\208\184\209\129\209\140 \208\190\209\130\208\186\209\128\209\139\209\130\208\176 \208\180\208\187\209\143 \208\177\208\181\208\187\208\190\208\179\208\190 \209\129\208\191\208\184\209\129\208\186\208\176\\\"> \",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"208892563\":{\"postid\":\"208892563\",\"dateline_date\":\"1461510276\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"\208\178\209\129\208\181 \209\141\209\130\208\190 \208\178\208\184\208\180\209\143\209\130\209\141\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1461510276\",\"message_html\":\"\208\178\209\129\208\181 \209\141\209\130\208\190 \208\178\208\184\208\180\209\143\209\130\209\141\",\"title\":\"\",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"208814688\":{\"postid\":\"208814688\",\"dateline_date\":\"1460924718\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"\209\131\208\182\208\176\209\129\208\189\208\190 \209\130\208\181\209\129\209\130\208\184\209\128\208\190\208\178\208\176\209\130\209\140 \209\130\208\176\208\188 \208\179\208\180\208\181 $EDITOR - vi:wq\\n\\n\\n\\n\\n:w\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1460924718\",\"message_html\":\"\209\131\208\182\208\176\209\129\208\189\208\190 \209\130\208\181\209\129\209\130\208\184\209\128\208\190\208\178\208\176\209\130\209\140 \209\130\208\176\208\188 \208\179\208\180\208\181 $EDITOR - vi:wq<br><br><br><br><br>:w\",\"title\":\"\",\"author_title\":\"\",\"tags_data\":{},\"can_edit\":\"1\"},\"208640193\":{\"postid\":\"208640193\",\"dateline_date\":\"1459699718\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"c \208\177\209\128\208\176\209\131\208\183\208\181\209\128\208\176\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459699718\",\"message_html\":\"c \208\177\209\128\208\176\209\131\208\183\208\181\209\128\208\176\",\"tags_data\":{\"293587\":\"&amp;#12473\",\"211846\":\"&amp;#12486\",\"211852\":\"&amp;#12488\"},\"title\":\"\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208640163\":{\"postid\":\"208640163\",\"dateline_date\":\"1459699555\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&amp;#12486;&amp;#12473;&amp;#12488;\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459699555\",\"message_html\":\"&#12486;&#12473;&#12488;\",\"tags_data\":{\"148067\":\"\208\178\209\130\208\190\209\128\208\176\209\143 \209\130\208\181\208\188\208\176\",\"5421714\":\"\208\191\208\181\209\128\208\178\208\176 \209\130\208\181\208\188\208\176\",\"5421715\":\"\208\189\208\184\209\135\208\181\208\179\208\190 \208\189\208\181 \209\129\208\187\208\190\208\188\208\176\208\189\208\190? \208\190\209\130\208\180\208\181\208\187\208\184\208\188 \209\129\208\187\208\181\208\180\209\131\209\142\209\137\209\131\209\142 \208\180\208\178\208\190\208\181\209\130\208\190\209\135\208\184\208\181\208\188\",\"1125\":\"\209\130\208\181\209\129\209\130\"},\"title\":\"&#12486;&#12473;&#12488;\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639886\":{\"postid\":\"208639886\",\"dateline_date\":\"1459698049\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&amp;#12486;&amp;#12473;&amp;#12488;\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459698049\",\"message_html\":\"&#12486;&#12473;&#12488;\",\"tags_data\":{\"293587\":\"&amp;#12473\",\"211846\":\"&amp;#12486\",\"211852\":\"&amp;#12488\",\"5421704\":\"dd&amp;#12486\"},\"title\":\"&#12486;&#12473;&#12488;\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639884\":{\"postid\":\"208639884\",\"dateline_date\":\"1459698041\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&amp;#12486;&amp;#12473;&amp;#12488;\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459698041\",\"message_html\":\"&#12486;&#12473;&#12488;\",\"tags_data\":{\"293587\":\"&amp;#12473\",\"211846\":\"&amp;#12486\",\"211852\":\"&amp;#12488\",\"5421704\":\"dd&amp;#12486\"},\"title\":\"&#12486;&#12473;&#12488;\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639866\":{\"postid\":\"208639866\",\"dateline_date\":\"1459697949\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&amp;#12486;&amp;#12473;&amp;#12488;\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459697949\",\"message_html\":\"&#12486;&#12473;&#12488;\",\"tags_data\":{\"293587\":\"&amp;#12473\",\"211852\":\"&amp;#12488\",\"5421704\":\"dd&amp;#12486\"},\"title\":\"&#12486;&#12473;&#12488;\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639856\":{\"postid\":\"208639856\",\"dateline_date\":\"1459697901\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&amp;#12486;&amp;#12473;&amp;#12488;\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459697901\",\"message_html\":\"&#12486;&#12473;&#12488;\",\"tags_data\":{\"293587\":\"&amp;#12473\",\"211852\":\"&amp;#12488\",\"5259\":\"d\",\"5421704\":\"dd&amp;#12486\"},\"title\":\"&#12486;&#12473;&#12488;\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639750\":{\"postid\":\"208639750\",\"dateline_date\":\"1459697436\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&#12486&#12473&#12488\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459697436\",\"message_html\":\"&#12486&#12473&#12488\",\"tags_data\":{\"5421684\":\"dd&amp;#12486&amp;#12473&amp;#12488d\"},\"title\":\"&amp;#12486&amp;#12473&amp;#12488\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639727\":{\"postid\":\"208639727\",\"dateline_date\":\"1459697360\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&#12486&#12473&#12488\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459697360\",\"message_html\":\"&#12486&#12473&#12488\",\"tags_data\":{\"5421684\":\"dd&amp;#12486&amp;#12473&amp;#12488d\"},\"title\":\"&amp;#12486&amp;#12473&amp;#12488\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639635\":{\"postid\":\"208639635\",\"dateline_date\":\"1459696899\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&#12486&#12473&#12488\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459696899\",\"message_html\":\"&#12486&#12473&#12488\",\"tags_data\":{\"5421684\":\"dd&amp;#12486&amp;#12473&amp;#12488d\"},\"title\":\"&amp;#12486&amp;#12473&amp;#12488\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639617\":{\"postid\":\"208639617\",\"dateline_date\":\"1459696811\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&#12486&#12473&#12488\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459696811\",\"message_html\":\"&#12486&#12473&#12488\",\"tags_data\":{\"5421622\":\"&amp;#12486&amp;#12473&amp;#12488\"},\"title\":\"&amp;#12486&amp;#12473&amp;#12488\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208639537\":{\"postid\":\"208639537\",\"dateline_date\":\"1459696369\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&#12486&#12473&#12488\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459696369\",\"message_html\":\"&#12486&#12473&#12488\",\"tags_data\":{\"5421622\":\"&amp;#12486&amp;#12473&amp;#12488\"},\"title\":\"&amp;#12486&amp;#12473&amp;#12488\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208637748\":{\"postid\":\"208637748\",\"dateline_date\":\"1459687037\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&#12486&#12473&#12488\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459687037\",\"message_html\":\"&#12486&#12473&#12488\",\"tags_data\":{\"5421622\":\"&amp;#12486&amp;#12473&amp;#12488\"},\"title\":\"&amp;#12486&amp;#12473&amp;#12488\",\"author_title\":\"\",\"can_edit\":\"1\"},\"208637071\":{\"postid\":\"208637071\",\"dateline_date\":\"1459682995\",\"juserid\":\"3374983\",\"shortname\":\"hastest\",\"author_userid\":\"3374983\",\"author_shortname\":\"hastest\",\"author_username\":\"hastest\",\"subscribed\":\"0\",\"avatarid\":\"0\",\"message_src\":\"&amp;#12486;&amp;#12473;&amp;#12488;\",\"access\":\"0\",\"close_access_mode\":\"0\",\"close_access_mode2\":\"0\",\"jaccess\":\"0\",\"dateline_cdate\":\"1459682995\",\"message_html\":\"&#12486;&#12473;&#12488;\",\"tags_data\":{\"211852\":\"&amp;#12488\",\"211846\":\"&amp;#12486\",\"293587\":\"&amp;#12473\"},\"title\":\"&#12486;&#12473;&#12488;\",\"author_title\":\"\",\"can_edit\":\"1\"}}}"

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
                                                     100
                                                     "12" 
                                                     "test" 
                                                     "TEST" 
                                                     "user"
                                                     (Just "user_journal"))

    it "decodes PostList" $ do 
        eitherDecode JsonSpec.posts `shouldBe` (Right $ IJ.PostList $
                                                  [IJ.Post "13" 
                                                     100
                                                     "12" 
                                                     "test" 
                                                     "TEST" 
                                                     "user"
                                                     (Just "user_journal"),
                                                   IJ.Post "12" 
                                                     100
                                                     "12" 
                                                     "test" 
                                                     "TEST" 
                                                     "user"
                                                     (Just "user_journal")])
    -- it "decodes real postList" $ do
    --     eitherDecode long `shouldBe` (Right $ IJ.PostList $
    --                                               [IJ.Post "13" 
    --                                                  "100"
    --                                                  "12" 
    --                                                  "test" 
    --                                                  "TEST" 
    --                                                  "user"
    --                                                  (Just "user_journal"),
    --                                                IJ.Post "12" 
    --                                                  "100"
    --                                                  "12" 
    --                                                  "test" 
    --                                                  "TEST" 
    --                                                  "user"
    --                                                  (Just "user_journal")])

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