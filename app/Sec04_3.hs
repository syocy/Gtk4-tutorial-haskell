{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.GI.Base (AttrOp ((:=)), new, on, set, asA)
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

clickCb :: (?self :: Gtk.Button) => Gtk.Window -> IO ()
clickCb win = do
  win.destroy

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  win <- new Gtk.ApplicationWindow [#application := ?self]

  win `set` [#title := "lb3"]
  win `set` [#defaultWidth := 400, #defaultHeight := 300]

  btn <- new Gtk.Button [#label := "Close"]
  
  win `set` [#child := btn]
  _ <- on btn #clicked (clickCb (win `asA` Gtk.Window))

  win.present

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec04-3"]
  _ <- on app #activate appActivate
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
