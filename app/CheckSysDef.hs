module Main
    ( main
    ) where

import           CSIM.Launcher
import           Options.Applicative

data Options = Options
    { sysPath :: !FilePath
    } deriving Show

main :: IO ()
main = do
    opts <- getOptions
    eSys <- systemDefinitionFromFile $ sysPath opts
    putStrLn `either` putStrLn $ eSys >>= checkSystemDefinition

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options =
    info (helper <*> optParser)
        (  fullDesc
        <> progDesc "Check CSIM system definition"
        <> header "System definition checker"
        )

optParser :: Parser Options
optParser =
    Options <$> strOption
                (  long "sys-path"
                <> short 'y'
                <> metavar "<SYSTEM DEFINITION FILE PATH>"
                <> help "File path to the system definition"
                )
