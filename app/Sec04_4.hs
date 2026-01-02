{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.GI.Base (AttrOp ((:=)), asA, get, new, on, set)
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

click1Cb :: (?self :: Gtk.Button) => IO ()
click1Cb = do
  s <- ?self `get` #label
  if s == (Just "Hello.")
    then ?self `set` [#label := "Good-bye."]
    else ?self `set` [#label := "Hello."]
  pure ()

click2Cb :: (?self :: Gtk.Button) => Gtk.Window -> IO ()
click2Cb win = do
  win.destroy

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  win <- new Gtk.ApplicationWindow [#application := ?self]

  win `set` [#title := "lb4"]
  win `set` [#defaultWidth := 400, #defaultHeight := 300]

  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 5]
  box `set` [#homogeneous := True]
  win `set` [#child := box]

  btn1 <- new Gtk.Button [#label := "Hello."]
  _ <- on btn1 #clicked click1Cb

  btn2 <- new Gtk.Button [#label := "Close"]
  _ <- on btn2 #clicked (click2Cb (win `asA` Gtk.Window))

  box.append btn1
  box.append btn2

  win.present

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec04-4"]
  _ <- on app #activate appActivate
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
