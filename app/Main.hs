{-# LANGUAGE  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


import qualified Data.ByteString.Lazy.Char8 as BL (ByteString)


import qualified Data.Configurator as C
import Control.Lens ((&))
import Api

import Options
import Utils





 

mainOptParse :: IO ()
mainOptParse = do 
  command <- execParser $ mainParser 
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
      parseOpt p@(Post _ _ _ _ _) client = createPost p client
      parseOpt s@(Send _ _ _ _ _) client = sendUmail s client
      parseOpt _  client              = umailGet client [] 





main :: IO ()
main = do

  print "OK"
  mainOptParse