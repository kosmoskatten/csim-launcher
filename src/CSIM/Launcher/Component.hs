{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module CSIM.Launcher.Component
    ( Component (..)
    ) where

import           CSIM.Launcher.Range (Range, Version)
import           Data.Aeson
import           Data.HashMap.Lazy   (HashMap)
import           Data.Text
import           GHC.Generics        (Generic)

type Provides     = HashMap Text Version
type Dependencies = HashMap Text Range

data Component = Component
    { name         :: !Text
    , summary      :: !Text
    , version      :: !Version
    , provides     :: !Provides
    , dependencies :: !Dependencies
    } deriving (Show, Generic, FromJSON)
