{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.GI.Base (AttrOp ((:=)), new, on, set)
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  win <- new Gtk.Window []
  set win [#application := ?self]
  win.present

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec03-3"]
  _ <- on app #activate appActivate
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
