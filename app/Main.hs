{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base (AttrOp (On, (:=)), new, on, set)
import GI.Gio qualified as Gio
import GI.Gtk qualified as Gtk

activate :: Gtk.Application -> IO ()
activate app = do
  button <-
    new
      Gtk.Button
      [ (#label := "button"),
        On
          #clicked
          ( ?self
              `set` [ #sensitive := False,
                      #label := "label"
                    ]
          )
      ]
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := "title",
        #defaultWidth := 400,
        #defaultHeight := 300,
        #child := button
      ]
  window.show

main :: IO ()
main = do
  app <-
    new
      Gtk.Application
      [ #applicationId := "com.example.test",
        On #activate (activate ?self)
      ]

  _ <- app.run Nothing

  pure ()