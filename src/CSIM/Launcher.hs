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

startSpecificationFromFile :: FilePath -> IO (Either String StartSpecification)
startSpecificationFromFile file = eitherDecode <$> LBS.readFile file

systemDefinitionFromFile :: FilePath -> IO (Either String SystemDefinition)
systemDefinitionFromFile file = eitherDecode <$> LBS.readFile file

calculateStartSet :: SystemDefinition -> StartSpecification
                  -> Either String [Component]
calculateStartSet sys start =
    expandRequirements sys =<< findCompatibleComponents sys start

-- | From the 'StartSpecification' and the 'SystemDefinition' find a set of
-- 'Component's that is matching the requirements in the 'StartSpecification'.
findCompatibleComponents :: SystemDefinition -> StartSpecification
                         -> Either String [Component]
findCompatibleComponents sys start =
    findCompatibleComponents' (components sys) $ HM.toList $ requires start

findCompatibleComponents' :: [Component] -> [(Text, Range)] -> Either String [Component]
findCompatibleComponents' cs rs = unique <$> mapM (findCompatibleComponent cs) rs

findCompatibleComponent :: [Component] -> (Text, Range)
                        -> Either String Component
findCompatibleComponent [] (service, _)           =
    Left $ printf "No component provides service '%s'" (unpack service)
findCompatibleComponent (x:xs) s@(service, range) =
    case HM.lookup service (provides x) of
        Just version'
            | version' `inRange` range -> Right x
            | otherwise                ->
                Left $
                    printf "Incompatible service found for '%s'. Requires '%s', but provided is '%s'"
                           (unpack service) (show range) (show version')
        Nothing      -> findCompatibleComponent xs s

expandRequirements :: SystemDefinition -> [Component] -> Either String [Component]
expandRequirements sys = go 0
  where
    go :: Int -> [Component] -> Either String [Component]
    go n cs = do
        cs' <- doExpansion cs
        let lcs = length cs'
        if lcs > n
            then go lcs cs'
            else Right cs'

    doExpansion :: [Component] -> Either String [Component]
    doExpansion cs = snd <$> foldM expandRequirement (components sys, cs) cs

expandRequirement :: ([Component], [Component]) -> Component -> Either String ([Component], [Component])
expandRequirement (sys, selected) component = do
    selected' <- findCompatibleComponents' sys (HM.toList $ dependsOn component)
    Right (sys, unique $ selected ++ selected')

unique :: [Component] -> [Component]
unique = nubBy (\x y -> name x == name y)
