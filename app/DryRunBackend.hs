module DryRunBackend
    ( DryRunBackend
    , create
    ) where

import           CSIM.Launcher (Backend (..), Component (..))
import           Text.Printf   (printf)

-- | This backend is just a constructor nothing more.
data DryRunBackend = DryRunBackend

create :: IO DryRunBackend
create = do
    printf "DryRunBackend created. Will provide no supervision\n"
    return DryRunBackend

instance Backend DryRunBackend where
    launch _ _ c =
        printf "Starting %s, version %s\n" (show $ name c) (show $ version c)
