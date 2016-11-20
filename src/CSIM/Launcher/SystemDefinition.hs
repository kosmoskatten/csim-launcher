{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module CSIM.Launcher.SystemDefinition
    ( SystemDefinition (..)
    ) where

import           CSIM.Launcher.Component (Component)
import           Data.Aeson              (FromJSON, ToJSON)
import           GHC.Generics            (Generic)

data SystemDefinition = SystemDefinition
    { components :: ![Component]
    } deriving (Show, Generic, FromJSON, ToJSON)
