{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.GI.Base (AttrOp ((:=)), get, new, on, set)
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  let text =
        "Once upon a time, there was an old man who was called Taketori-no-Okina. "
          <> "It is a japanese word that means a man whose work is making bamboo baskets.\n"
          <> "One day, he went into a hill and found a shining bamboo. "
          <> "\"What a mysterious bamboo it is!,\" he said. "
          <> "He cut it, then there was a small cute baby girl in it. "
          <> "The girl was shining faintly. "
          <> "He thought this baby girl is a gift from Heaven and took her home.\n"
          <> "His wife was surprized at his story. "
          <> "They were very happy because they had no children. "

  win <- new Gtk.ApplicationWindow [#application := ?self]
  win `set` [#title := "Taketori"]
  win `set` [#defaultWidth := 400, #defaultHeight := 300]

  tv <- new Gtk.TextView []
  tb <- tv `get` #buffer
  tb.setText text (-1)
  tv `set` [#wrapMode := Gtk.WrapModeWordChar]

  win `set` [#child := tv]

  win.present

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec05-1"]
  _ <- on app #activate appActivate
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
