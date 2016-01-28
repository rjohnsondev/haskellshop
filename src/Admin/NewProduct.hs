{-# LANGUAGE OverloadedStrings #-}

module Admin.NewProduct where

import Admin.Feedback
import Application
import Control.Applicative
import Control.Monad.Trans
import Data.Scientific
import FormUtil
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist.Compiled
import Snap.Util.FileUploads
import Text.Printf
import Codec.ImageType
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as M
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified ShopData.Product as P
import qualified ShopData.ProductDetail as PD

productPriceFromPost :: Int -> Handler App App (Int, Scientific)
productPriceFromPost voId = do
        p <- getFormScientific (C8.pack (printf "price-%d" voId)) 0.0
        return (voId, p)

productFromPost :: Handler App App (P.Product, [Int], [(Int, Scientific)])
productFromPost = do
        pid           <- getFormInt "product-id" 0
        name         <- getFormByteString "product-name" ""
        manufacturer <- getFormByteString "product-manufacturer" ""
        price        <- getFormScientific "product-base-price" 0.0
        cats         <- getFormIntArray "categories"
        vos          <- getFormIntArray "product-variant-options"
        pvos         <- mapM productPriceFromPost vos
        enabled      <- getFormBool "product-enabled"
        let p = P.Product {P.productId = pid,
                           P.name = DTE.decodeUtf8 name,
                           P.manufacturer = DTE.decodeUtf8 manufacturer,
                           P.basePrice = price,
                           P.enabled = enabled}
        return (p, cats, pvos)

deleteImageSubmit :: Handler App App ()
deleteImageSubmit = do
        pid <- getFormInt "product-id" 0
        hash <- getFormByteString "image-delete" ""
        _ <- with db $ P.deleteImage pid hash
        infoRedirect
            (C8.pack (printf "/admin/new_product?product_id=%d" pid))
            "Image deleted"

imageMove :: Bool -> Handler App App ()
imageMove down = do
        pid <- getFormInt "product-id" 0
        let key = if down then "image-move-down" else "image-move-up"
        hash <- getFormByteString key ""
        _ <- with db $ P.moveImage pid down hash
        infoRedirect
            (C8.pack (printf "/admin/new_product?product_id=%d" pid))
            "Image moved"

handleNewProductSubmit :: [(PartInfo, Either PolicyViolationException FilePath)] -> Handler App App ()
handleNewProductSubmit files = do
        (p, cats, pvos) <- productFromPost
        pid <- with db $ P.saveProduct p
        _ <- with db $ P.saveCategories pid cats
        _ <- with db $ P.saveProductVariantOptions pid pvos
        detail <- getFormByteString "product-details" ""
        _ <- if detail /= ""
                then with db $ PD.saveProductDetail
                             $ PD.ProductDetail {PD.productDetailId = 0,
                                                 PD.productId = pid,
                                                 PD.title = "Product Details",
                                                 PD.detail = DTE.decodeUtf8 detail}
                else return 0
        (add, lvl, msg') <- handleSaveImage pid files
        if lvl == "info"
            then infoRedirect
                    (C8.pack (printf "/admin/new_product?product_id=%d" pid))
                    (DT.pack (printf "Product Saved (#%d)" pid))
            else msgRedirect lvl add msg'

handleSaveImageSubmit :: [(PartInfo, Either PolicyViolationException FilePath)]
                      -> Handler App App ()
handleSaveImageSubmit files = do
        pid <- getFormInt "product-id" 0
        (add, lvl, msg') <- handleSaveImage pid files
        msgRedirect lvl add msg'

handleSaveImage :: Int -> [(PartInfo, Either PolicyViolationException FilePath)]
                -> Handler App App (BS.ByteString, DT.Text, DT.Text)
handleSaveImage pid files = do
        let filtered = filter (\(p, _) -> case partFileName p of
                                              Nothing -> False
                                              Just f -> f /= "") files
        if pid == 0
            then return ("/admin/products", "danger", "Attempted to add image to non-existant product")
            else if length filtered == 0
                then return ((C8.pack (printf "/admin/new_product?product_id=%d" pid)),
                             "info",
                             "Please select an image")
                else do
                    let (_, e) = head filtered
                    case e of
                        Left _ -> return ((C8.pack (printf "/admin/new_product?product_id=%d" pid)),
                                          "danger",
                                          "Error uploading image")
                        Right fp -> do
                            valid <- liftIO $ isJpeg fp
                            if valid
                                then do
                                    with db $ P.addImage pid fp
                                    return ((C8.pack (printf "/admin/new_product?product_id=%d" pid)),
                                            "info",
                                            "Image saved")
                                else
                                    return ((C8.pack (printf "/admin/new_product?product_id=%d" pid)),
                                            "danger",
                                            "Only JPEG images are currently supported")

wrapFileUploads :: ([(PartInfo, Either PolicyViolationException FilePath)] -> Handler App App ())
                -> Handler App App ()
wrapFileUploads =
        handleFileUploads
            "/tmp"
            (setMaximumNumberOfFormInputs 50 defaultUploadPolicy)
            (const (allowWithMaximumSize 32000000))

handleSaveDetailSubmit :: Handler App App ()
handleSaveDetailSubmit = do
        pdid <- getFormInt "product-detail-save" 0
        productId <- getFormInt "product-id" 0
        title <- getFormByteString (C8.pack (printf "product-detail-title-%d" pdid)) ""
        detail <- getFormByteString (C8.pack (printf "product-detail-detail-%d" pdid)) ""
        let pd = PD.ProductDetail {PD.productDetailId = pdid,
                                   PD.productId = productId,
                                   PD.title = DTE.decodeUtf8 title,
                                   PD.detail = DTE.decodeUtf8 detail}
        with db $ PD.saveProductDetail pd
        info "Product details updated"
        redirect (C8.pack (printf "/admin/new_product?product_id=%d" productId))

deleteDetailSubmit :: Handler App App ()
deleteDetailSubmit = do
        pdid <- getFormInt "product-detail-delete" 0
        productId <- getFormInt "product-id" 0
        with db $ PD.delProductDetail pdid
        info "Product details deleted"
        redirect (C8.pack (printf "/admin/new_product?product_id=%d" productId))

detailMove :: Bool -> Handler App App ()
detailMove down = do
        productId <- getFormInt "product-id" 0
        let key = if down then "product-detail-move-down" else "product-detail-move-up"
        pdid <- getFormInt key 0
        with db $ PD.moveProductDetail down productId pdid
        infoRedirect
            (C8.pack (printf "/admin/new_product?product_id=%d" productId))
            "Detail moved"

handleNewProduct :: Handler App App ()
handleNewProduct =
        method GET handleNewProductGet <|>
        method POST (wrapFileUploads handleNewProductPost)
    where
        handler p files
            | "image-move-down" `M.member` p     = imageMove True
            | "image-move-up" `M.member` p       = imageMove False
            | "image-delete" `M.member` p        = deleteImageSubmit
            | "product-detail-save" `M.member` p = handleSaveDetailSubmit
            | "product-detail-delete" `M.member` p = deleteDetailSubmit
            | "product-detail-move-up" `M.member` p = detailMove False
            | "product-detail-move-down" `M.member` p = detailMove True
            | "product-save" `M.member` p        = handleNewProductSubmit files
            | "product-save-image" `M.member` p  = handleSaveImageSubmit files
            | otherwise                          = redirect "/admin/products"
        handleNewProductGet = render "admin/_product"
        handleNewProductPost files = do
            p <- getParams
            handler p files
