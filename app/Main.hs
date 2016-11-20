{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           Control.Concurrent  (threadDelay)
import           Control.Monad       (forever, void)
import           CSIM.Launcher
import           Data.Aeson          (ToJSON)
import           GHC.Generics        (Generic)
import           Network.Nats
import           Options.Applicative
import           Text.Printf         (printf)

data Options = Options
    { natsUri :: !String
    , sysPath :: !FilePath
    , dryRun  :: !Bool
    } deriving Show

data LauncherReply = LauncherReply
    { status :: !Int
    } deriving (Show, Generic, ToJSON)

main :: IO ()
main = do
    opts <- getOptions
    either putStrLn (runLauncher opts)
        =<< (systemDefinitionFromFile $ sysPath opts)

runLauncher :: Options -> SystemDefinition -> IO ()
runLauncher opts sys =
    withNats defaultSettings [natsUri opts] $ \nats -> do
        void $ subscribeAsync nats "sys.launcher.applyStartSpec"
                              Nothing (startSpecHandler nats)
        keepAlive

startSpecHandler :: Nats -> Msg -> IO ()
startSpecHandler nats msg =
    maybe handleDecodeError applyStartSpec decodeStartSpec
  where
    handleDecodeError :: IO ()
    handleDecodeError =
        maybe (return ())
              (\topic' ->
                  publishJson nats topic' Nothing LauncherReply { status = 401 }
              ) $ replyTo msg

    applyStartSpec :: StartSpecification -> IO ()
    applyStartSpec start = do
        print start
        maybe (return ())
              (\topic' ->
                  publishJson nats topic' Nothing LauncherReply { status = 200 }
              ) $ replyTo msg

    decodeStartSpec :: Maybe StartSpecification
    decodeStartSpec = jsonPayload msg

keepAlive :: IO ()
keepAlive = forever $ threadDelay 1000000

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
    info (helper <*> optParser)
         (  fullDesc
         <> progDesc "Start the CSIM launcher service"
         <> header "CSIM LAUNCHER"
         )

optParser :: Parser Options
optParser =
    Options <$> strOption
                (  long "nats"
                <> short 'n'
                <> metavar "<NATS URI>"
                <> value "nats://localhost:4222"
                <> help "NATS URI for connecting to server (default: nats://localhost:4222)"
                )
            <*> strOption
                (  long "sys"
                <> short 's'
                <> metavar "<SYSTEM DEFINITION PATH>"
                <> help "Path to the system definition"
                )
            <*> switch
                (  long "dry-run"
                <> short 'd'
                <> help "Dry run"
                )
