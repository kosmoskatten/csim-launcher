{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module CSIM.Launcher.StartSpecification
    ( StartSpecification (..)
    ) where

import           CSIM.Launcher.Range (Range)
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.HashMap.Lazy   (HashMap)
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

type Requires = HashMap Text Range

data StartSpecification = StartSpecification
    { requires :: !Requires
    } deriving (Show, Generic, FromJSON, ToJSON)
