{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception.Safe (try)
import Control.Monad (forM_, when)
import Data.GI.Base (AttrOp ((:=)), GError, get, new, on, set)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  hPutStrLn stderr "You need filename arguments."

appOpen :: (?self :: Gtk.Application) => [Gio.File] -> T.Text -> IO ()
appOpen files _ = do
  win <- new Gtk.ApplicationWindow [#application := ?self]
  win `set` [#title := "file viewer"]
  win `set` [#defaultWidth := 600, #defaultHeight := 400]

  nb <- new Gtk.Notebook []
  win `set` [#child := nb]

  forM_ files \file -> do
    result <- try $ file.loadContents (Nothing @Gio.Cancellable)
    case result of
      Right (contents, _) -> do
        scr <- new Gtk.ScrolledWindow []
        tv <- new Gtk.TextView []
        tb <- tv `get` #buffer
        tv `set` [#wrapMode := Gtk.WrapModeWordChar, #editable := False]
        scr `set` [#child := tv]

        let contentsText = T.decodeUtf8 contents
        tb.setText contentsText (fromIntegral $ T.length contentsText)
        filenameMaybe <- file.getBasename
        lab <- case filenameMaybe of
          Just filename -> new Gtk.Label [#label := T.pack filename]
          Nothing -> new Gtk.Label []
        _ <- nb.appendPage scr (Just lab)
        nbp <- nb.getPage scr
        nbp `set` [#tabExpand := True]
      Left (err :: GError) -> do
        message <- Gtk.gerrorMessage err
        T.hPutStrLn stderr message

  n <- nb.getNPages
  if n > 0
    then win.present
    else win.destroy

main :: IO ()
main = do
  argv <- (:) <$> getExecutablePath <*> getArgs -- Reconstruct C-style argv
  app <- new Gtk.Application [#applicationId := "com.example.sec07-2", #flags := [Gio.ApplicationFlagsHandlesOpen]]
  _ <- on app #activate appActivate
  _ <- on app #open appOpen
  stat <- app.run $ Just argv
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
