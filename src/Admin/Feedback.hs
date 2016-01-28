{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoMonomorphismRestriction, GADTs #-}

module Admin.Feedback where

import Application
import Data.ByteString (ByteString)
import Data.Text
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Text.Printf
import qualified Data.ByteString.Char8 as C8
import qualified Control.Monad.CatchIO as CIO
import qualified Control.Exception as CE

msg :: Text -> Text -> Handler App App ()
msg level m = with sess $ do
    setInSession level m
    commitSession

msgRedirect :: Text -> ByteString -> Text -> Handler App App ()
msgRedirect level url m = do
    msg level m
    redirect url

info :: Text -> Handler App App ()
info = msg "info"

infoRedirect :: ByteString -> Text -> Handler App App ()
infoRedirect = msgRedirect "info"

danger :: Text -> Handler App App ()
danger = msg "danger"

dangerRedirect :: ByteString -> Text -> Handler App App ()
dangerRedirect = msgRedirect "danger"

catchErrors :: HasHeist b => Handler b v () -> Handler b v ()
catchErrors action = action `CIO.catch` (\(ex::CE.SomeException) -> do
                  logError $ C8.pack (printf "Unexpected exception: %s" (show ex))
                  modifyResponse $ setResponseCode 500
                  render "err")
