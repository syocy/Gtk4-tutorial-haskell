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
  win <- new Gtk.ApplicationWindow [#application := ?self]
  set win [#title := "lb1"]
  set win [#defaultWidth := 400, #defaultHeight := 300]
  lab <- new Gtk.Label [#label := "Hello."]
  set win [#child := lab]
  win.present

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec04-1"]
  _ <- on app #activate appActivate
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
