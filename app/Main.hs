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
import           DryRunBackend       (create)
import           GHC.Generics        (Generic)
import           Network.Nats
import           Options.Applicative

data Options = Options
    { natsUri :: !String
    , sysPath :: !FilePath
    , dryRun  :: !Bool
    } deriving Show

data Runtime a = Runtime
    { sys     :: !SystemDefinition
    , backend :: !a
    }

data LauncherReply = LauncherReply
    { status :: !Int
    , errMsg :: !(Maybe String)
    } deriving (Show, Generic, ToJSON)

main :: IO ()
main = do
    opts <- getOptions
    either putStrLn (runLauncher opts)
        =<< (systemDefinitionFromFile $ sysPath opts)

runLauncher :: Options -> SystemDefinition -> IO ()
runLauncher opts sys' = do
    backend' <- create
    let runtime = Runtime { sys     = sys'
                          , backend = backend'
                          }
    withNats defaultSettings [natsUri opts] $ \nats -> do
        void $ subscribeAsync nats "sys.launcher.applyStartSpec"
                              Nothing (startSpecHandler runtime nats)
        keepAlive

startSpecHandler :: Backend a => Runtime a -> Nats -> Msg -> IO ()
startSpecHandler rt nats msg =
    maybe handleDecodeError applyStartSpec decodeStartSpec
  where
    handleDecodeError :: IO ()
    handleDecodeError =
        maybe (return ())
              (\topic' ->
                  publishJson nats topic'
                              Nothing
                              LauncherReply { status = 401
                                            , errMsg = Just "Cannot decode JSON"
                                            }
              ) $ replyTo msg

    applyStartSpec :: StartSpecification -> IO ()
    applyStartSpec start = do
        let startSet = calculateStartSet (sys rt) start
        reply <-
            case startSet of
                Right cs -> do
                    mapM_ (launch (backend rt) Nothing) cs
                    return LauncherReply { status = 200
                                         , errMsg = Nothing
                                         }
                Left err -> return LauncherReply { status = 409
                                                 , errMsg = Just err
                                                 }
        maybe (return ())
              (\topic' ->
                  publishJson nats topic' Nothing reply
              ) $ replyTo msg

    decodeStartSpec :: Maybe StartSpecification
    decodeStartSpec = jsonPayload msg

--mkBackend :: Options -> IO (Runtime a)
--mkBackend _ = Runtime <$> create

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
