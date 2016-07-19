module Module.Import.Preprocess where

import Control.Monad
import qualified Data.Char as Char
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath
import System.Directory

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = (f <$>) <$> a

process :: FilePath -> Text -> IO Text
process src content =
  Text.unlines . concat <$> mapM transformLine (Text.lines content)
  where
    transformLine :: Text -> IO [Text]
    transformLine = either (pure . pure) (\r -> importModule r <$$> getSubModules srcDir r) . pluckBaseModule
    srcDir = fst $ splitFileName src

pluckBaseModule :: Text -> Either Text Text
pluckBaseModule line =
   maybe (Left line) Right $ (Text.stripPrefix "import " <=< Text.stripSuffix ".*") line

getSubModules :: FilePath -> Text -> IO [Text]
getSubModules srcDir baseModule =
  mapMaybe ((".hs" `Text.stripSuffix`) . Text.pack) <$> getDirectoryContents (getRoot srcDir </> pathify baseModule)


-- | This uses the gross heuristic that upper cases letters are the start of your module hierarchy
-- e.g. In @src/foo/Bar/Baz.hs@, the module is @Bar.Baz@
getRoot :: FilePath -> FilePath
getRoot = joinPath . takeWhile (not . Char.isUpper . head)  . splitPath

pathify :: Text -> String
pathify = foldr1 (</>) . map Text.unpack . Text.splitOn "."

importModule :: Text -> Text -> Text
importModule baseModule subModule =
  "import " <> baseModule <> "." <> subModule <> " as Export"
