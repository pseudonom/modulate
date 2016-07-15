{-# OPTIONS_GHC -F -pgmF modulate #-}

module Lib where

import Lib.

someFunc :: IO ()
someFunc = putStrLn "someFunc"
  where
    () = foo
    () = bar
