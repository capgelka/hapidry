{-# LANGUAGE  OverloadedStrings #-}

module Options
  ( mainParser
  , Commands(..)
  , Auth(..)
  , Args(..)
  , execParser
    ) where

import Options.Applicative
import Data.List.Split (splitOn)
import Data.Char (isSpace)


type Action = String
type Target = String
type UserId = String
type Path   = String
data Auth   = Auth (Maybe String) (Maybe String) deriving (Show)
type ConfigPath = String

data Args = Args { auth :: Auth, config :: ConfigPath, commands :: Commands } deriving (Show)

data Commands 
    = Notify
      {
        quiet :: Bool,
        all  :: Bool,
        umail :: Bool,
        discussion :: Bool,
        comment :: Bool,
        count :: Bool
      }
    | Post 
      { 
        blog :: [Target],
        text :: Maybe String,
        title :: Maybe String,
        file :: Maybe Path,
        pipe :: Bool,
        themes :: [String]
      }
    | Send
      {
        user :: [Target],
        title :: Maybe String,
        text :: Maybe String,
        file :: Maybe Path,
        pipe :: Bool
      } deriving (Show)

mainParser :: ParserInfo Args
mainParser = parseArgs `withInfo` "diary.ru API client"     

parseArgs :: Parser Args
parseArgs = Args <$> parseAuth <*> parseConfig <*> parseCommands

parseConfig :: Parser ConfigPath
parseConfig = strOption $
                short 'c'
                <> long "config"
                <> value "$(HOME)/.hapidry"
                <> metavar "CONFIG"
                <> help "path to config file"

parseAuth :: Parser Auth
parseAuth = Auth <$> optional 
              (strOption $
                short 'u'
                <> long "user"
                <> metavar "LOGIN"
                <> help "user login")
              <*> optional 
                   (strOption $
                     short 'p'
                     <> long "password"
                     <> metavar "PASSWORD"
                     <> help "user password")

parseCommands :: Parser Commands
parseCommands = subparser $
    command "notify" (parseNotify `withInfo` "get notification data") <>
    command "post"  (parsePost  `withInfo` "create new post") <>
    command "send"  (parseSend  `withInfo` "send new umail")


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseNotify :: Parser Commands
parseNotify = Notify
    <$> switch
      (long "quiet"
        <> short 'q'
        <> help "show something only if it's count greter then 0")
    <*> switch
      (long "count"
       <> short 'c'
       <> help "show only count")
    <*> switch
      (long "umail"
       <> short 'U'
       <> help "show umails notifiactions on")
    <*> switch
      (long "comment"
       <> short 'C'
       <> help "show comments on")
    <*> switch
      (long "discussion"
       <> short 'D'
       <> help "show discussions on")
    <*> switch
      (long "all"
       <> short 'a'
       <> help "show all")

-- parseUser :: Parser Commands
-- parseUser = User 
--     <$> argument str (metavar "USER_ACTION")
--     <*> argument str (metavar "USER_UID")

parseSend :: Parser Commands
parseSend = Send
    <$> many (argument str (metavar "UMAIL_USERNAME"))
    <*> (optional $ strOption $
        short 'm'
        <> long "message"
        <> metavar "UMAIL_MESSAGE")
    <*> (optional $ strOption $
        short 't'
        <> long "title"
        <> metavar "UMAIL_MESSAGE_TITLE")
    <*> (optional $ strOption $
        short 'f'
        <> long "file"
        <> metavar "UMAIL_MESSAGE_FILE")
    <*> (switch
      (long "pipe"
       <> short 'p'
       <> help "get text from stdin"))

parsePost :: Parser Commands
parsePost = Post 
    <$> many (argument str (metavar "BLOG"))
    <*> (optional $ strOption $
        short 'm'
        <> long "message"
        <> metavar "POST_MESSAGE")
    <*> (optional $ strOption $
        short 't'
        <> long "title"
        <> metavar "POST_MESSAGE_TITLE")
    <*> (optional $ strOption $
        short 'f'
        <> long "file"
        <> metavar "POST_MESSAGE_FILE")
    <*> (switch
      (long "pipe"
       <> short 'p'
       <> help "get text from stdin"))
    <*> (multiString $
        short 'T'
        <> long "tags"
        <> metavar "POST_MESSAGE_TAGS")
      

multiString desc = concat <$> many single where 
  single = option ((map trim . splitOn ",") <$> str) desc
  trim :: String -> String
  trim = f . f
  f = reverse . dropWhile isSpace
