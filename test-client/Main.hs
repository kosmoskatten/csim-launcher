{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           CSIM.Launcher       (StartSpecification,
                                      startSpecificationFromFile)
import           Network.Nats
import           Options.Applicative

data Options = Options
    { natsUri  :: !String
    , filePath :: !FilePath
    } deriving Show

main :: IO ()
main = do
    opts <- getOptions
    either putStrLn (runCommand opts)
        =<< (startSpecificationFromFile $ filePath opts)

runCommand :: Options -> StartSpecification -> IO ()
runCommand opts start =
    withNats defaultSettings [natsUri opts] $ \nats -> do
        msg <- requestJson nats "sys.launcher.applyStartSpec" start
        print $ payload msg

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
    info (helper <*> optParser)
         (  fullDesc
         <> progDesc "CSIM Launcher Test Client"
         <> header "CSIM LAUNCHER TEST CLIENT"
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
                (  long "start"
                <> short 's'
                <> metavar "<CSIM START SPECIFICATION>"
                <> help "Path to the start definition"
                )
