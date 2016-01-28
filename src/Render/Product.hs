{-# LANGUAGE OverloadedStrings #-}

module Render.Product where

import HeistUtil
import Application
import Control.Monad.Trans
import FormUtil
import Heist
import Snap.Snaplet
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import qualified Heist.Compiled as C
import qualified ShopData.Product as P
import qualified ShopData.ProductDetail as PD
import qualified Text.Blaze.Html.Renderer.Text as HRT
import qualified Text.Markdown as MD
import qualified Text.XmlHtml as X

renderProductDetails :: Splices (RuntimeSplice AppHandler (Maybe P.Product) -> C.Splice AppHandler)
renderProductDetails =
    do
        "name"        ## pt (\x -> case x of Nothing -> ""
                                             Just y -> P.name y)
        "manufacturer" ## pt (\x -> case x of Nothing -> ""
                                              Just y -> P.manufacturer y)
        "id"          ## pt (\x -> case x of Nothing -> "0"
                                             Just y -> DT.pack . show . P.productId $ y)
        "base_price"  ## pt (\x -> case x of Nothing -> "0.00"
                                             Just y -> DT.pack . formatPrice $ P.basePrice y)
        "enabled_checkbox" ## templatedNodeSplice . renderProductEnabledCheckbox
        "hide_new_product_class"  ## pt (\x -> case x of Nothing -> ""
                                                         Just _ -> "display: none;")
        "hide_product_images"  ## pt (\x -> case x of Nothing -> "display: none;"
                                                      Just _ -> "")
        "images" ## imagesSplice
        "image_hash" ## imageHash
        "details" ## detailsSplice
    where
        pt = C.pureSplice . escapingTextSplice

renderImage :: Splices (RuntimeSplice AppHandler DT.Text -> C.Splice AppHandler)
renderImage = do "hash" ## C.pureSplice . escapingTextSplice $ id

renderDetails :: Splices (RuntimeSplice AppHandler (PD.ProductDetail, Bool) -> C.Splice AppHandler)
renderDetails = do
        "title"        ## pt $ (\(pd, _) -> PD.title pd)
        "detail_html"  ## C.pureSplice . C.textSplice $ (\(pd, _) -> DTL.toStrict . HRT.renderHtml
                                                                     $ MD.markdown MD.def
                                                                     (DTL.fromStrict $ PD.detail pd))
        "detail"       ## pt $ (\(pd, _) -> PD.detail pd)
        "active_class" ## pt $ (\(_, a) -> if a then "active" else "")
        "id"           ## pt $ (\(pd, _) -> DT.pack . show . PD.productDetailId $ pd)
    where
        pt = C.pureSplice . escapingTextSplice

images :: Maybe P.Product -> RuntimeSplice AppHandler [DT.Text]
images mp = case mp of Nothing -> return []
                       Just p -> lift $ with db $ P.images (P.productId p)

imageHash :: RuntimeSplice AppHandler (Maybe P.Product) -> C.Splice AppHandler
imageHash x = return . C.yieldRuntimeText
                $ (x >>= (\mp -> case mp of Nothing -> return ""
                                            Just p -> img p))
    where
        img p = do imgs <- lift $ with db $ P.images (P.productId p)
                   if null imgs then return ""
                                else return $ head imgs

details :: Maybe P.Product -> RuntimeSplice AppHandler [(PD.ProductDetail, Bool)]
details mp = 
        case mp of Nothing -> return []
                   Just p -> do pds <- lift $ with db $ P.details (P.productId p)
                                return $ activateFirst pds
    where
        eq pd pd' = (PD.productDetailId pd == PD.productDetailId pd')
        activateFirst pds = map (\pd -> (pd, eq pd (head pds))) pds

imagesSplice :: RuntimeSplice AppHandler (Maybe P.Product) -> C.Splice AppHandler
imagesSplice x = C.manyWithSplices C.runChildren renderImage $ (x >>= images)
        
detailsSplice :: RuntimeSplice AppHandler (Maybe P.Product) -> C.Splice AppHandler
detailsSplice x = C.manyWithSplices C.runChildren renderDetails $ (x >>= details)

renderProductEnabledCheckbox :: RuntimeSplice AppHandler (Maybe P.Product)
                             -> X.Node
                             -> RuntimeSplice AppHandler X.Node
renderProductEnabledCheckbox catm n = catm >>= \mp ->
        return $ case mp of Nothing -> n
                            Just p -> if P.enabled p
                                        then X.setAttribute "checked" "" n
                                        else n

renderProduct :: RuntimeSplice AppHandler (Maybe P.Product) -> C.Splice AppHandler
renderProduct = C.withSplices C.runChildren renderProductDetails

productRuntime :: RuntimeSplice AppHandler (Maybe P.Product)
productRuntime = do
        i <- lift $ getFormInt "product_id" 0
        if i == 0 then return $ Nothing
                  else lift $ with db $ P.getProduct i
