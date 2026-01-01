{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base (AttrOp ((:=)), new, on)
import GI.Gtk qualified as Gtk

activate :: (?self :: Gtk.Application) => IO ()
activate = do
  label <-
    new
      Gtk.Label
      [#label := "hello"]

  window <-
    new
      Gtk.ApplicationWindow
      [ #title := "hello",
        #defaultWidth := 400,
        #defaultHeight := 300,
        #application := ?self,
        #child := label
      ]

  window.present

  pure ()

main :: IO ()
main = do
  app <-
    new
      Gtk.Application
      [#applicationId := "com.example.test"]

  _ <- on app #activate activate

  _ <- app.run Nothing

  pure ()
