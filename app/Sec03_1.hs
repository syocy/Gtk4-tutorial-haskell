{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Sec03_1 where

import Control.Monad (when)
import Data.GI.Base (AttrOp ((:=)), new)
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec03-1"]
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
