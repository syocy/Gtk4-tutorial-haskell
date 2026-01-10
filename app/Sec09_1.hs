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
import Data.GI.Base (AttrOp ((:=)), get, new, on, set)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Foreign.Ptr qualified as P
import Foreign.StablePtr qualified as P
import GHC.Records (HasField (..))
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

data TfeTextViewState = TfeTextViewState
  { tfeFile :: Gio.File
  }
  deriving (Eq)

instance HasField "file" TfeTextViewState Gio.File where
  getField :: TfeTextViewState -> Gio.File
  getField = tfeFile

tfeTextViewStateKey :: T.Text
tfeTextViewStateKey = "com.example.sec08-1.tfetextview.state"

newTfeTextViewStatePtr :: TfeTextViewState -> IO (P.Ptr ())
newTfeTextViewStatePtr = (fmap P.castStablePtrToPtr) . P.newStablePtr

deRefTfeTextViewStatePtr :: P.Ptr () -> IO TfeTextViewState
deRefTfeTextViewStatePtr = P.deRefStablePtr . P.castPtrToStablePtr

freeTfeTextViewStatePtr :: P.Ptr () -> IO ()
freeTfeTextViewStatePtr =
  -- There is no need to restore type info when freeing a StablePtr
  P.freeStablePtr . P.castPtrToStablePtr

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  hPutStrLn stderr "You need to give filenames as arguments."

appOpen :: (?self :: Gtk.Application) => [Gio.File] -> T.Text -> IO ()
appOpen files _ = do
  win <- new Gtk.ApplicationWindow [#application := ?self]
  win `set` [#title := "file editor"]
  win `set` [#defaultWidth := 600, #defaultHeight := 400]

  boxv <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 0]
  win `set` [#child := boxv]

  boxh <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 0]
  boxv.append boxh

  dmy1 <- new Gtk.Label [#widthChars := 10]
  dmy2 <- new Gtk.Label [#hexpand := True]
  dmy3 <- new Gtk.Label [#widthChars := 10]
  btnn <- new Gtk.Button [#label := "New"]
  btno <- new Gtk.Button [#label := "Open"]
  btns <- new Gtk.Button [#label := "Save"]
  btnc <- new Gtk.Button [#label := "Close"]

  boxh.append dmy1
  boxh.append btnn
  boxh.append btno
  boxh.append dmy2
  boxh.append btns
  boxh.append btnc
  boxh.append dmy3

  nb <- new Gtk.Notebook []
  nb `set` [#hexpand := True, #vexpand := True]
  boxv.append nb

  forM_ files \file -> do
    contentsEither <- try $ file.loadContents (Nothing @Gio.Cancellable)
    _ <- case contentsEither of
      Right (bs, _) -> do
        scr <- new Gtk.ScrolledWindow []
        tv <- new Gtk.TextView []
        tb <- tv `get` #buffer
        tv `set` [#wrapMode := Gtk.WrapModeWordChar]
        scr `set` [#child := tv]

        newState <- TfeTextViewState <$> file.dup
        newStatePtr <- newTfeTextViewStatePtr newState
        _ <- tv.setDataFull tfeTextViewStateKey newStatePtr $ Just freeTfeTextViewStatePtr

        let contentsText = T.decodeUtf8 bs
        _ <- tb.setText contentsText (fromIntegral $ T.length contentsText)

        filename <- (T.pack . fromJust) <$> file.getBasename
        lab <- new Gtk.Label [#label := filename]
        _ <- nb.appendPage scr (Just lab)
        nbp <- nb.getPage scr
        nbp `set` [#tabExpand := True]
      Left err -> T.hPutStrLn stderr =<< Gtk.gerrorMessage err

    pure ()

  n <- nb.getNPages
  if n > 0
    then win.present
    else win.destroy

main :: IO ()
main = do
  argv <- (:) <$> getExecutablePath <*> getArgs -- Reconstruct C-style argv
  app <- new Gtk.Application [#applicationId := "com.example.sec09-1", #flags := [Gio.ApplicationFlagsHandlesOpen]]
  _ <- on app #activate appActivate
  _ <- on app #open appOpen
  stat <- app.run $ Just argv
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
