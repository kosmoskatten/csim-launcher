{-# LANGUAGE OverloadedStrings #-}
module CSIM.Launcher.Range
    ( Cmp (..)
    , Range (..)
    , Version (..)
    , inRange
    ) where

import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import           Data.Attoparsec.Text

type Op = (Version -> Version -> Bool)

data Cmp = CmpLT | CmpLE | CmpGT | CmpGE
    deriving Show

data Range = Range !Version !Cmp !Cmp !Version
    deriving Show

-- | It seem that default derivation of Ord is ok for the handling of
-- sematic versions. Perhaps quick-check stuff?
data Version = Version !Int !Int !Int
    deriving (Eq, Ord, Show)

instance FromJSON Range where
    parseJSON (String str) =
        either fail return $ parseOnly range str
    parseJSON invalid      = typeMismatch "Range" invalid

inRange :: Range -> Version -> Bool
inRange (Range low cmp1 cmp2 high) v =
    low `op1` v && v `op2` high
  where
     op1 = toOp cmp1
     op2 = toOp cmp2

toOp :: Cmp -> Op
toOp CmpLT = (<)
toOp CmpLE = (<=)
toOp CmpGT = (>)
toOp CmpGE = (>=)

range :: Parser Range
range = range' <* (skipSpace >> endOfInput)
  where
    range' =
        Range <$> (skipSpace *> version)
              <*> (skipSpace *> cmp)
              <*> ((skipSpace *> char 'v') *> (skipSpace *> cmp))
              <*> (skipSpace *> version)

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

cmp :: Parser Cmp
cmp = choice [ string ">=" *> pure CmpGE
             , string "<=" *> pure CmpLE
             , string ">"  *> pure CmpGT
             , string "<"  *> pure CmpLT
             ]
