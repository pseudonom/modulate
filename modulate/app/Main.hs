module Main where

import qualified Data.Text.IO as Text
import System.Environment (getArgs)

import Module.Import.Preprocess

main :: IO ()
main = do
  args <- getArgs
  case args of
    [src, _, dst] -> Text.writeFile dst =<< process src =<< Text.readFile src
    _ -> error "modulate does not support options"
