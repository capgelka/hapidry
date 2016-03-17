{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)

import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Char8 as B

import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)

import Options.Applicative

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Control.Lens ((&))
import Text.Editor (runUserEditor)
import Api

import Options




applyOptions :: [(Text, Maybe String)] -> [(Text, Text)]
applyOptions = map (\(x, Just y) -> (x, T.pack $ y)) . filter (\(_, y) -> y /= Nothing)

updateCreds :: ClientCredentials -> Auth -> ClientCredentials
updateCreds  client (Auth Nothing Nothing)   = client
updateCreds  client (Auth Nothing (Just x))  = client { password = B.pack x } 
updateCreds  client (Auth (Just x) Nothing)  = client { username = T.pack x }
updateCreds  client (Auth (Just x) (Just y)) = client { username = T.pack x,
                                                        password = B.pack y } 

readOption :: CT.Config -> CT.Name -> IO Text
readOption conf opt = (readOption' <$> (C.lookup conf opt) ) where
    readOption' (Just x) = x
    readOption' Nothing  = ""

readOptionB :: CT.Config -> CT.Name -> IO B.ByteString
readOptionB conf opt = encodeUtf8 <$> readOption conf opt


createPost :: Commands -> ClientCredentials -> IO (Either Integer BL.ByteString)
createPost (Post _ title (Just x) False) client = do
    text <- readFile x
    postCreate client (applyOptions
                      [("message", Just text),
                       ("title", title)])    
createPost (Post _ title _ True) client = do 
  text <- getContents
  postCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)])
createPost (Post Nothing title Nothing False) client = do 
  text <- T.unpack <$> decodeUtf8 <$> runUserEditor
  postCreate client  (applyOptions
                      [("message", Just text),
                       ("title", title)])
createPost (Post text title _ _) client = postCreate client 
                                                    (applyOptions
                                                     [("message", text),
                                                      ("title", title)])


sendUmail :: Commands -> ClientCredentials -> IO (Either Integer BL.ByteString)
sendUmail (Send user _ title (Just x) False) client = do
    text <- readFile x
    umailSend client (applyOptions
                      [("username", Just user),
                       ("message", Just text),
                       ("title", title)])    
sendUmail (Send user _ title _ True) client = do 
    text <- getContents
    umailSend client  (applyOptions
                        [("username", Just user),
                         ("message", Just text),
                         ("title", title)])
sendUmail (Send user Nothing title Nothing False) client = do 
    text <- T.unpack <$> decodeUtf8 <$> runUserEditor
    umailSend client  (applyOptions
                        [("username", Just user),
                         ("message", Just text),
                         ("title", title)])  
sendUmail (Send user text title _ _) client = umailSend client 
                                                    (applyOptions
                                                     [("username", Just user),
                                                      ("message", text),
                                                      ("title", title)])
 

mainOptParse :: IO ()
mainOptParse = do 
  command <- execParser $ mainParser --(parseArgs 
  --                         `withInfo` "diary.ru API client")
  print command
  cfg <- C.load [C.Required (command & config)]
  password <- readOptionB cfg "password"
  username <- readOption cfg "username"
  client <- authRequest $ ClientCredentials {
                password = password,
                appkey  = "5ab793910e36584cd81622e5eb77d3d1",
                sid     = Right "",
                username    = username,  
                secret  = "8543db8deccb4b0fcb753291c53f8f4f"
              } & updateCreds $ command & auth
  print command                     
  parseOpt (command & commands) client >>= print where
      parseOpt :: Commands -> ClientCredentials -> IO (Either Integer BL.ByteString)
      parseOpt (Umail "get" _) client = umailGet client []  
      parseOpt p@(Post _ _ _ _) client = createPost p client
      parseOpt s@(Send _ _ _ _ _) client = sendUmail s client
      parseOpt _  client              = umailGet client [] 





main :: IO ()
main = do

  print "OK"
  mainOptParse