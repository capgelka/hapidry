{-# LANGUAGE  OverloadedStrings, TemplateHaskell, ExistentialQuantification #-}

module Options
  ( mainParser
  , Commands(..)
  , Auth(..)
  , Args(..)
  , customExecParser
  , Folder(..)
  , parserPrefs
    ) where

import Options.Applicative
import Options.Applicative.Builder (eitherReader, infoOption)
import Data.List.Split (splitOn)
import Data.Char (isSpace)


import Data.Monoid 


type Action = String
type Target = String
type Id = String
type Path   = String
data Auth   = Auth (Maybe String) (Maybe String) deriving (Show)
type ConfigPath = String
type Version =  Bool
data Folder = Input | Output | Deleted deriving (Enum, Show)


parserPrefs = prefs $ disambiguate
                    <> showHelpOnError
                    <> showHelpOnEmpty


 -- ParserPrefs { prefDisambiguate = True,
 --                      prefShowHelpOnError = True,
 --                      prefShowHelpOnEmpty = True }

data Args = Args { auth :: Auth, 
                   config :: ConfigPath, 
                   versionFlag :: Bool,
                   commands :: Commands} deriving (Show)

folderReader :: ReadM Folder
folderReader = eitherReader $ \arg -> case arg of
    "output"  -> Right Output
    "input"   -> Right Input
    "deleted" -> Right Deleted
    _         -> Left "wrong dolder name"

data Commands 
    = None 
    | Notify
      {
        quiet :: Bool,
        all  :: Bool,
        umail :: Bool,
        discussion :: Bool,
        comment :: Bool,
        count :: Bool
      }
    | Comment 
      {
        post :: Id, -- ^ post id
        text :: Maybe String, -- ^ optional field to store message text 
        file :: Maybe Path, -- ^ optional field for path to file with message for comment
        pipe :: Bool -- ^ flag. read message from stdin uf set
      }
    | Post 
      { 
        blog :: [Target], -- ^ list of blognames to create post in
        text :: Maybe String, -- ^ optional field to store message text 
        title :: Maybe String, -- ^ optional field to store post title
        file :: Maybe Path, -- ^ optional field for path to file with message for post
        pipe :: Bool, -- ^ flag. read message from stdin uf set
        whitelist :: Bool, -- ^flag. close post for whitelist only if set.
        draft :: Bool, -- ^flag. post to draft if set  
        themes :: [String] -- ^ list of tags for post
      }
    | Send
      {
        user :: [Target], -- ^ list of u-mail receptiens
        title :: Maybe String, -- ^ optional field to store u-mail title
        text :: Maybe String, -- ^ optional field to store message text 
        file :: Maybe Path, -- ^ optional field for path to file with message for u-mail
        pipe :: Bool -- ^ flag. read message from stdin uf set
      } 
    | Blog 
      {
        blognames :: [Target],  -- ^ list of blognames to read
        reversed :: Bool -- ^ reverse order of post by date if set
      }
    | Umail
      {
        folder :: Maybe Folder, -- ^ folder to read
        reversed :: Bool -- ^ reverse order of post by date if set
      } deriving (Show)

mainParser :: ParserInfo Args
mainParser = parseArgs `withInfo` "diary.ru API client" 

parseArgs :: Parser Args
parseArgs = Args <$> parseAuth 
                 <*> parseConfig 
                 <*>  switch (long "version"
                   <> short 'v'
                   <> help "show version")
                 <*> (parseCommands <|> pure None)
                 -- <|> DefaultCommand


-- parseVersion :: Parser Version
-- parseVersion = infoOption 
--                 (concat [showVersion version, " ", $(gitHash)])
--                 (short 'v'
--                  <> long "version"
--                  <> help "path to config file")

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
    command "comment" (parseComment  `withInfo` "create new comment") <>
    command "send"  (parseSend  `withInfo` "send new umail") <>
    command "read" (parseRead `withInfo` "read blogposts") <>
    command "umail" (parseUmail `withInfo` "read umail")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseRead :: Parser Commands
parseRead = Blog
    <$> many (argument str (metavar "BLOG_NAME"))
    <*> switch
      (long "reversed"
       <> short 'r'
       <> help "reverse sorting order")
    -- <*> switch
    --   (long "umail"
    --    <> short 'U'
    --    <> help "show umails notifiactions on")
    -- <*> switch
    --   (long "comment"
    --    <> short 'C'
    --    <> help "show comments on")
    -- <*> switch
    --   (long "discussion"
    --    <> short 'D'
    --    <> help "show discussions on")
    -- <*> switch
    --   (long "all"
    --    <> short 'a'
    --    <> help "show all")

-- | Subparser for hapidry notify
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

-- | Subparser for hapidry comment
parseComment :: Parser Commands
parseComment = Comment
    <$> argument str (metavar "POST_ID")
    <*> (optional $ strOption $
        short 'm'
        <> long "message"
        <> metavar "COMMENT_MESSAGE")
    <*> (optional $ strOption $
        short 'f'
        <> long "file"
        <> metavar "COMMENT_MESSAGE_FILE")
    <*> (switch
      (long "pipe"
       <> short 'p'
       <> help "get text from stdin"))

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

parseUmail :: Parser Commands
parseUmail = Umail
    <$> (optional $ option folderReader $
        short 'f'
        <> long "folders"
        <> metavar "UMAIL_FOLDER")
    <*> switch
        (long "reversed"
         <> short 'r'
         <> help "reverse sorting order")


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
    <*> (switch
      (long "whitelist"
       <> short 'w'
       <> help "make post visible only to whitelist"))
    <*> (switch
      (long "draft"
       <> short 'd'
       <> help "save post as draft"))
    <*> (multiString
        (short 'T'
        <> long "tags"
        <> metavar "POST_MESSAGE_TAGS"
        <> help "add tags to message"))
      

multiString desc = concat <$> many single where 
  single = option ((map trim . splitOn ",") <$> str) desc
  trim :: String -> String
  trim = f . f
  f = reverse . dropWhile isSpace
