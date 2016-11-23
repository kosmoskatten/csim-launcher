module Main
    ( main
    ) where

import           CSIM.Launcher
import           Options.Applicative
import           Text.Printf         (printf)

data Options = Options
    { sysPath   :: !FilePath
    , startPath :: !FilePath
    } deriving Show

main :: IO ()
main = do
    opts   <- getOptions
    eSys   <- systemDefinitionFromFile $ sysPath opts
    eStart <- startSpecificationFromFile $ startPath opts
    putStrLn `either` applyStartSpec $ (,) <$> eSys <*> eStart

applyStartSpec :: (SystemDefinition, StartSpecification) -> IO ()
applyStartSpec (sys, start) =
    putStrLn `either` mapM_ printComponent $ calculateStartSet sys start
  where
    printComponent :: Component -> IO ()
    printComponent c = printf "%s -> %s\n" (show $ name c) (show $ version c)

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
    info (helper <*> optParser)
        (  fullDesc
        <> progDesc "Apply CSIM start spec to the system definition"
        <> header "Start spec application"
        )

optParser :: Parser Options
optParser =
    Options <$> strOption
                (  long "sys-path"
                <> short 'y'
                <> metavar "<SYSTEM DEFINITION FILE PATH>"
                <> help "File path to the system definition"
                )
            <*> strOption
                (  long "start-path"
                <> short 's'
                <> metavar "<START SPECIFICATION FILE PATH>"
                <> help "File path to the start specification"
                )
