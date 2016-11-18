module CSIM.Launcher
    ( Component (..)
    , Range (..)
    , StartSpecification (..)
    , SystemDefinition (..)
    , Version (..)
    , startSpecificationFromFile
    , systemDefinitionFromFile
    ) where

import           CSIM.Launcher.Component          (Component (..))
import           CSIM.Launcher.Range              (Range (..), Version (..))
import           CSIM.Launcher.StartSpecification (StartSpecification (..))
import           CSIM.Launcher.SystemDefinition   (SystemDefinition (..))
import           Data.Aeson                       (eitherDecode)
import qualified Data.ByteString.Lazy.Char8       as LBS

startSpecificationFromFile :: FilePath -> IO (Either String StartSpecification)
startSpecificationFromFile file = eitherDecode <$> LBS.readFile file

systemDefinitionFromFile :: FilePath -> IO (Either String SystemDefinition)
systemDefinitionFromFile file = eitherDecode <$> LBS.readFile file
