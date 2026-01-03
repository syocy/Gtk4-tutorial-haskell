{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception.Safe (try)
import Control.Monad (when)
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
  hPutStrLn stderr "You need a filename argument."

appOpen :: (?self :: Gtk.Application) => [Gio.File] -> T.Text -> IO ()
appOpen files _ = do
  win <- new Gtk.ApplicationWindow [#application := ?self]
  win `set` [#defaultWidth := 400, #defaultHeight := 300]

  scr <- new Gtk.ScrolledWindow []
  win `set` [#child := scr]

  tv <- new Gtk.TextView []
  tb <- tv `get` #buffer
  tv `set` [#wrapMode := Gtk.WrapModeWordChar, #editable := False]
  scr `set` [#child := tv]

  let file = files !! 0
  result <- try $ file.loadContents (Nothing @Gio.Cancellable)
  case result of
    Right (contents, _) -> do
      let contentsText = T.decodeUtf8 contents
      tb.setText contentsText (fromIntegral $ T.length contentsText)
      win.present
    Left (err :: GError) -> do
      message <- Gtk.gerrorMessage err
      T.hPutStrLn stderr message
      win.destroy

main :: IO ()
main = do
  argv <- (:) <$> getExecutablePath <*> getArgs -- Reconstruct C-style argv
  app <- new Gtk.Application [#applicationId := "com.example.sec07-1", #flags := [Gio.ApplicationFlagsHandlesOpen]]
  _ <- on app #activate appActivate
  _ <- on app #open appOpen
  stat <- app.run $ Just argv
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
