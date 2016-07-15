module Module.Import.Preprocess where

import Control.Monad
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
   maybe (Left line) Right $ (Text.stripPrefix "import " <=< Text.stripSuffix ".") line

getSubModules :: FilePath -> Text -> IO [Text]
getSubModules srcDir baseModule =
  mapMaybe ((".hs" `Text.stripSuffix`) . Text.pack) <$> getDirectoryContents (srcDir </> Text.unpack baseModule)

importModule :: Text -> Text -> Text
importModule baseModule subModule =
  "import " <> baseModule <> "." <> subModule
