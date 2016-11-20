{-# LANGUAGE OverloadedStrings #-}
module CSIM.Launcher.Range
    ( Range (..)
    , Version (..)
    , inRange
    ) where

import           Control.Monad        (void)
import           Data.Aeson           (FromJSON (..), ToJSON (..), Value (..))
import           Data.Aeson.Types     (typeMismatch)
import           Data.Attoparsec.Text (Parser, char, choice, decimal,
                                       endOfInput, parseOnly, skipSpace, string)
import           Data.Text            (pack)
import           Text.Printf          (printf)

data Range = Range !Version !Version

-- | It seem that default derivation of Ord is ok for the handling of
-- semantic versions. Perhaps quick-check stuff?
data Version = Version !Int !Int !Int
    deriving (Eq, Ord)

instance Show Range where
    show (Range low high) = printf "%s <= v < %s" (show low) (show high)

instance Show Version where
    show (Version major minor patch) = printf "%d.%d.%d" major minor patch

instance FromJSON Range where
    parseJSON (String str) =
        either fail return $ parseOnly range str
    parseJSON invalid      = typeMismatch "Range" invalid

instance ToJSON Range where
    toJSON = String . pack . show

instance FromJSON Version where
    parseJSON (String str) =
        either fail return $ parseOnly version str
    parseJSON invalid      = typeMismatch "Version" invalid

instance ToJSON Version where
    toJSON = String . pack . show

inRange :: Version -> Range -> Bool
inRange v (Range low high) = low <= v && v < high

range :: Parser Range
range = range' <* (skipSpace >> endOfInput)
  where
    range' = do
        low <- skipSpace *> version
        void $ skipSpace *> string "<="
        void $ skipSpace *> char 'v'
        void $ skipSpace *> char '<'
        high <- skipSpace *> version
        return $ Range low high

version :: Parser Version
version = choice [longVersion, shortVersion]

longVersion :: Parser Version
longVersion =
    Version <$> versionDigit <* char '.'
            <*> versionDigit <* char '.'
            <*> versionDigit

shortVersion :: Parser Version
shortVersion =
    Version <$> versionDigit <* char '.'
            <*> versionDigit
            <*> pure 0

versionDigit :: Parser Int
versionDigit = decimal
