module CSIM.Launcher.Backend
    ( Backend (..)
    ) where

import           CSIM.Launcher.Component (Component)

-- | Type class defining the interface of a launcher backend.
class Backend a where
    -- | Launch the given component. Optionally provide a callback
    -- function used for signal component termination or erroneuos start.
    launch :: a -> Maybe (Component -> IO ()) -> Component -> IO ()
