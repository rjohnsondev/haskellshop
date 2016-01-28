{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, NoMonomorphismRestriction, GADTs #-}

module Render.Alerts where

import Application
import Data.Text
import Heist
import Snap.Snaplet
import Snap.Snaplet.Session
import Control.Monad.Trans
import qualified Heist.Compiled as C

getSessionValues :: [Text] -> AppHandler [(Text, Text)]
getSessionValues = 
        Prelude.foldl
            (\out level -> do
                v <- out
                s <- with sess $ getFromSession level
                _ <- with sess $ deleteFromSession level
                return $ case s of Just sv -> v ++ [(level, sv)]
                                   Nothing -> v)
            (return [])

alertRuntime :: RuntimeSplice AppHandler [(Text, Text)]
alertRuntime = lift $ getSessionValues ["danger", "info"]
        
splicesFromAlert :: Splices (RuntimeSplice AppHandler (Text, Text) -> C.Splice AppHandler)
splicesFromAlert = do "level" ## C.pureSplice $ C.textSplice fst
                      "msg"   ## C.pureSplice $ C.textSplice snd

renderAlerts :: RuntimeSplice AppHandler [(Text, Text)] -> C.Splice AppHandler
renderAlerts = C.manyWithSplices C.runChildren splicesFromAlert


