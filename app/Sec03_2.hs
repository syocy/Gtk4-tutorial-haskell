{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad (when)
import Data.GI.Base (AttrOp ((:=)), new, on)
import GI.Gtk qualified as Gtk
import System.Exit (ExitCode (ExitFailure), exitWith)

appActivate :: (?self :: Gtk.Application) => IO ()
appActivate = do
  putStrLn "GtkApplication is activated."

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "com.example.sec03-2"]
  _ <- on app #activate appActivate
  stat <- app.run Nothing
  when (stat /= 0) do
    exitWith (ExitFailure $ fromIntegral stat)
  pure ()
