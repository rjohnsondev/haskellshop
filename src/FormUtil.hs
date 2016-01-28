{-# LANGUAGE OverloadedStrings #-}

module FormUtil where

import Snap
import Application
import Data.Scientific
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as DT
import qualified Data.Char as DC
import qualified Data.Text.Encoding as DTE

getFormValue :: (Read a, Num a)
             => BS.ByteString
             -> a
             -> Handler App App a
getFormValue k d = do
        params <- getParams
        let vs' = DTE.decodeUtf8 $ head $ M.findWithDefault [""] k params
        let vs = if DT.length vs' > 0 && DC.isDigit (DT.head vs') == False
                      then DT.tail vs'
                      else vs'
        return $ case reads (DT.unpack vs) :: Read a => [(a, String)] of
                     [] -> d
                     [(v, _)] -> v
                     _ -> d

getFormInt :: BS.ByteString
           -> Int
           -> Handler App App Int
getFormInt = getFormValue

getFormIntArray :: BS.ByteString -> Handler App App [Int]
getFormIntArray k = do
        params <- getParams
        let r = M.findWithDefault [] k params
        return $ bsArrayToInt r
    where
        bsArrayToInt =
            foldr (\x out -> case reads (C8.unpack x) :: [(Int, String)] of
                               []        -> out
                               [(xi, _)] -> xi : out
                               _         -> out)
                  ([] :: [Int])

getFormScientific :: BS.ByteString
                  -> Scientific
                  -> Handler App App Scientific
getFormScientific = getFormValue

getFormByteString :: BS.ByteString
           -> BS.ByteString
           -> Handler App App BS.ByteString
getFormByteString k d = do
        params <- getParams
        return $ head $ M.findWithDefault [d] k params

getFormBool :: BS.ByteString -> Handler App App Bool
getFormBool k = do
        params <- getParams
        return $ M.member k params

