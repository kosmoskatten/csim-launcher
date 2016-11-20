module CSIM.Launcher
    ( Component (..)
    , Range (..)
    , StartSpecification (..)
    , SystemDefinition (..)
    , Version (..)
    , startSpecificationFromFile
    , systemDefinitionFromFile
    , calculateStartSet
    ) where

import           Control.Monad                    (foldM)
import           CSIM.Launcher.Component          (Component (..))
import           CSIM.Launcher.Range              (Range (..), Version (..),
                                                   inRange)
import           CSIM.Launcher.StartSpecification (StartSpecification (..))
import           CSIM.Launcher.SystemDefinition   (SystemDefinition (..))
import           Data.Aeson                       (eitherDecode)
import qualified Data.ByteString.Lazy.Char8       as LBS
import qualified Data.HashMap.Lazy                as HM
import           Data.List                        (nubBy)
import           Data.Text                        (Text, unpack)
import           Text.Printf                      (printf)

-- | Read a 'StartSpecification' from the provided file path.
startSpecificationFromFile :: FilePath -> IO (Either String StartSpecification)
startSpecificationFromFile file = eitherDecode <$> LBS.readFile file

-- | Read a 'SystemDefinition' from the provided file path.
systemDefinitionFromFile :: FilePath -> IO (Either String SystemDefinition)
systemDefinitionFromFile file = eitherDecode <$> LBS.readFile file

-- | Calculate a set of 'Component's to start from the 'SystemDefinition'
-- in order to satisify the 'StartSpecification'.
calculateStartSet :: SystemDefinition -> StartSpecification
                  -> Either String [Component]
calculateStartSet sys start = expandReqs sys =<< findComponents sys start

-- | From the 'StartSpecification' and the 'SystemDefinition' find a set of
-- 'Component's that is matching the requirements in the 'StartSpecification'.
findComponents :: SystemDefinition -> StartSpecification -> Either String [Component]
findComponents sys = findComponents' (components sys) . HM.toList . requires

-- | Really the implementation of 'findComponents'', but using simpler types.
-- The list have only unique components, with regards to their names.
findComponents' :: [Component] -> [(Text, Range)] -> Either String [Component]
findComponents' cs rs = unique <$> mapM (findComponent cs) rs

-- | Find a 'Component' that's providing the given service name and version.
findComponent :: [Component] -> (Text, Range) -> Either String Component

-- | No more components to search. Error.
findComponent [] (service, _)           =
    Left $ printf "No component provides service '%s'" (unpack service)

-- | Examine the 'Component' c. If c is providing a service matching the
-- contstrains then the search is done. If a provided service not is matching
-- with regards to versioning there's an error.
findComponent (c:cs) s@(service, range) =
    case HM.lookup service (provides c) of
        Just version'
            | version' `inRange` range -> Right c
            | otherwise                ->
                Left $
                    printf "Incompatible service found for '%s'. Requires '%s', but provided is '%s'"
                           (unpack service) (show range) (show version')
        Nothing      -> findComponent cs s

-- | Recursively expand the requirements for the components. Expand until
-- the list of required components no longer is growing.
expandReqs :: SystemDefinition -> [Component] -> Either String [Component]
expandReqs sys = go 0
  where
    go :: Int -> [Component] -> Either String [Component]
    go n cs = do
        cs' <- snd <$> foldM expandReq (components sys, cs) cs
        let lcs = length cs'
        if lcs > n
            then go lcs cs'
            else Right cs'

-- | Expand the requirements for the 'Component' component.
expandReq :: ([Component], [Component]) -> Component
          -> Either String ([Component], [Component])
expandReq (sys, selected) component = do
    selected' <- findComponents' sys (HM.toList $ dependsOn component)
    Right (sys, unique $ selected ++ selected')

-- | Make a list unique with regards to the names of the components.
unique :: [Component] -> [Component]
unique = nubBy (\x y -> name x == name y)
