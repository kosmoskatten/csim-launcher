{-# LANGUAGE OverloadedStrings #-}
module CSIM.Launcher.Range
    ( Range (..)
    , Version (..)
    , inRange
    ) where

import           Control.Monad        (void)
import           Data.Aeson           (FromJSON (..), Value (..))
import           Data.Aeson.Types     (typeMismatch)
import           Data.Attoparsec.Text (Parser, char, choice, decimal,
                                       endOfInput, parseOnly, skipSpace, string)

data Range = Range !Version !Version
    deriving Show

-- | It seem that default derivation of Ord is ok for the handling of
-- sematic versions. Perhaps quick-check stuff?
data Version = Version !Int !Int !Int
    deriving (Eq, Ord, Show)

instance FromJSON Range where
    parseJSON (String str) =
        either fail return $ parseOnly range str
    parseJSON invalid      = typeMismatch "Range" invalid

instance FromJSON Version where
    parseJSON (String str) =
        either fail return $ parseOnly version str
    parseJSON invalid      = typeMismatch "Version" invalid

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
