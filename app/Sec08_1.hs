{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception.Safe (catch, try)
import Control.Monad (forM_, when)
import Data.GI.Base (AttrOp ((:=)), GError, get, new, on, set, unsafeCastTo)
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

beforeClose :: (?self :: Gtk.ApplicationWindow) => Gtk.Notebook -> IO Bool
beforeClose nb = do
  n <- nb.getNPages
  forM_ [0 .. n - 1] \i -> do
    scr <- unsafeCastTo Gtk.ScrolledWindow =<< fromJust <$> nb.getNthPage i
    tv <- unsafeCastTo Gtk.TextView =<< fromJust <$> scr `get` #child
    stateData <- tv.getData tfeTextViewStateKey
    state <- deRefTfeTextViewStatePtr stateData
    let file = state.file
    tb <- tv `get` #buffer
    (start, end) <- tb.getBounds
    contentsText <- tb.getText start end False
    _ <- catch
      (file.replaceContents (T.encodeUtf8 contentsText) Nothing True [Gio.FileCreateFlagsNone] (Nothing @Gio.Cancellable))
      \(err :: Gtk.GError) -> do
        T.hPutStrLn stderr =<< Gtk.gerrorMessage err
        pure Nothing
    pure ()
  pure False

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  hPutStrLn stderr "You need to give filenames as arguments."

appOpen :: (?self :: Gtk.Application) => [Gio.File] -> T.Text -> IO ()
appOpen files _ = do
  win <- new Gtk.ApplicationWindow [#application := ?self]
  win `set` [#title := "file editor"]
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
        tv `set` [#wrapMode := Gtk.WrapModeWordChar, #editable := True]
        scr `set` [#child := tv]

        newState <- TfeTextViewState <$> file.dup
        newStatePtr <- newTfeTextViewStatePtr newState
        _ <- tv.setDataFull tfeTextViewStateKey newStatePtr $ Just freeTfeTextViewStatePtr

        let contentsText = T.decodeUtf8 contents
        _ <- tb.setText contentsText (fromIntegral $ T.length contentsText)
        filename <- T.pack . fromJust <$> file.getBasename
        lab <- new Gtk.Label [#label := filename]
        _ <- nb.appendPage scr (Just lab)
        nbp <- nb.getPage scr
        nbp `set` [#tabExpand := True]
      Left (err :: GError) -> do
        message <- Gtk.gerrorMessage err
        T.hPutStrLn stderr message

  n <- nb.getNPages
  if n > 0
    then do
      _ <- on win #closeRequest (beforeClose nb)
      win.present
    else win.destroy

main :: IO ()
main = do
  argv <- (:) <$> getExecutablePath <*> getArgs -- Reconstruct C-style argv
  app <- new Gtk.Application [#applicationId := "com.example.sec08-1", #flags := [Gio.ApplicationFlagsHandlesOpen]]
  _ <- on app #activate appActivate
  _ <- on app #open appOpen
  stat <- app.run $ Just argv
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
