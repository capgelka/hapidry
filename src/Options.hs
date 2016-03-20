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
    = Umail  {
        umailAction :: Action
      , target :: Maybe Target
      }
    | User
      {
        userAction :: Action
      , uid :: UserId
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
parseConfig = (strOption $
              short 'c'
              <> long "config"
              <> value "$(HOME)/.hapidry"
              <> metavar "CONFIG"
              <> help "path to config file")

parseAuth :: Parser Auth
parseAuth = Auth <$> (optional $ strOption $
              short 'u'
              <> long "user"
              <> metavar "LOGIN"
              <> help "user login")
            <*> (optional $ strOption $
                 short 'p'
                 <> long "password"
                 <> metavar "PASSWORD"
                 <> help "user password")

parseCommands :: Parser Commands
parseCommands = subparser $
    command "umail" (parseUmail `withInfo` "get/send umails") <>
    command "user"  (parseUser  `withInfo` "get user info") <>
    command "post"  (parsePost  `withInfo` "create new post") <>
    command "send"  (parseSend  `withInfo` "send new umail")


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseUser :: Parser Commands
parseUser = User 
    <$> argument str (metavar "USER_ACTION")
    <*> argument str (metavar "USER_UID")


parseUmail :: Parser Commands
parseUmail = Umail 
    <$> argument str (metavar "UMAIL_ACTION")
    <*> (optional $ strOption $
        short 'U'
        <> long "user"
        <> metavar "UMAIL_TARGET")

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
  single = option (str >>= return . map trim . (splitOn ",")) desc
  trim :: String -> String
  trim = f . f
  f = reverse . dropWhile isSpace