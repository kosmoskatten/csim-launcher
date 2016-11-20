{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module CSIM.Launcher.Component
    ( Component (..)
    ) where

import           CSIM.Launcher.Range (Range, Version)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.HashMap.Lazy   (HashMap)
import           Data.Text
import           GHC.Generics        (Generic)

type Provides     = HashMap Text Version
type DependsOn    = HashMap Text Range

data Component = Component
    { name      :: !Text
    , summary   :: !Text
    , version   :: !Version
    , provides  :: !Provides
    , dependsOn :: !DependsOn
    } deriving (Show, Generic, FromJSON, ToJSON)
