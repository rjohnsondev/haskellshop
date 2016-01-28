{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Admin.ProductVariants where

import Admin.Feedback
import Application
import Control.Applicative
import FormUtil
import Snap.Core
import Snap.Snaplet
import Text.Printf
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text.Encoding as DTE
import qualified ShopData.Variant as V
import qualified ShopData.VariantOption as VO
import Snap.Snaplet.Heist.Compiled


handleNewVariant :: Handler App App ()
handleNewVariant = do
        vn <- getFormByteString "variant-name" ""
        let v = V.Variant {V.variantId = 0,
                           V.name = DTE.decodeUtf8 vn,
                           V.adjustsPrice = False,
                           V.isSearchable = False,
                           V.options = []}
        with db $ V.saveVariant v
        infoRedirect "/admin/product_options" "New Variant Group Added"

handleNewVariantOption ::  Handler App App ()
handleNewVariantOption = do
        nid <- getFormInt "new-variant-option" 0
        nvo <- getFormByteString (C8.pack (printf "variant-option-%d" nid)) ""
        if nvo == ""
            then danger "Please enter a variant option value."
            else do with db $ VO.addVariantOption nid (DTE.decodeUtf8 nvo)
                    info "New Variant Option Added"
        redirect "/admin/product_options"

handleDelVariantOption :: Handler App App ()
handleDelVariantOption = do
        voId <- getFormInt "del-variant-option-id" 0
        with db $ VO.delVariantOption voId
        infoRedirect "/admin/product_options" "Variant Option Deleted"

handleDelVariant :: Handler App App ()
handleDelVariant = do
        vid <- getFormInt "del-variant-id" 0
        with db $ V.delVariant vid
        infoRedirect "/admin/product_options" "Variant Group Deleted"

handleProductVariants :: Handler App App ()
handleProductVariants =
        method GET handleProductOptGet <|>
        method POST handleProductOptPost
    where
        handleProductOptGet = render "admin/_variants"
        handler p
            | "del-variant-id" `M.member` p        = handleDelVariant
            | "del-variant-option-id" `M.member` p = handleDelVariantOption
            | "new-variant" `M.member` p           = handleNewVariant
            | otherwise                            = handleNewVariantOption
        handleProductOptPost = do
            params <- getParams
            handler params
