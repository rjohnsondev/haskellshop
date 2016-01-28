{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Render.Order where

import Application
import Control.Monad.Trans
import FormUtil
import Heist
import Heist.Compiled as C
import HeistUtil
import Snap.Snaplet
import Data.Scientific
import qualified Data.Text as DT
import qualified ShopData.Order as O
import qualified ShopData.OrderItem as OI
import qualified ShopData.OrderAddress as OA
import qualified ShopData.Product as P

renderAddress :: RuntimeSplice AppHandler OA.OrderAddress -> C.Splice AppHandler
renderAddress =
        C.withSplices
            C.runChildren
            (do "name"     ## pt $ OA.fullName
                "line_1"   ## pt $ OA.line1
                "line_2"   ## pt $ OA.line2
                "city"     ## pt $ OA.city
                "state"    ## pt $ OA.state
                "postcode" ## pt $ OA.postcode
                "country"  ## pt $ OA.country)
    where
        pt = C.pureSplice . escapingTextSplice

orderAddressRuntime :: OA.AddressType -> RuntimeSplice AppHandler OA.OrderAddress
orderAddressRuntime at = do
        oid <- lift $ getFormInt "order_id" 0
        lift $ with db $ OA.orderAddress oid at

type OrderItemViewModel = ( DT.Text -- product_name
                          , DT.Text -- product manufacturer
                          , DT.Text -- product_options
                          , DT.Text -- image hash
                          , Scientific) -- line total

productImage :: Int -> AppHandler DT.Text
productImage pid = do
        images <- with db $ P.images pid
        return $ case images of
                     [] -> ""
                     (hash:_) -> hash

renderItems :: RuntimeSplice AppHandler [OrderItemViewModel] -> C.Splice AppHandler
renderItems =
        C.manyWithSplices
            C.runChildren
            (do "name"         ## pt $ (\(n, _, _, _, _) -> n)
                "manufacturer" ## pt $ (\(_, m, _, _, _) -> m)
                "options"      ## pt $ (\(_, _, po, _, _) -> po)
                "price"        ## pt $ (\(_, _, _, _, t) -> DT.pack . formatPrice $ t)
                "hash"         ## pt $ (\(_, _, _, h, _) -> h))
    where
        pt = C.pureSplice . escapingTextSplice


orderItemRuntime :: RuntimeSplice AppHandler [OrderItemViewModel]
orderItemRuntime = do
        oid <- lift $ getFormInt "order_id" 0
        ois <- lift $ with db $ OI.orderItems oid
        foldr (\oi out' -> do
                  out <- out'
                  img <- lift $ productImage (OI.productId oi)
                  pm <- lift $ with db $ P.getProduct (OI.productId oi)
                  case pm of Nothing -> out'
                             Just p -> return $ ( P.name p
                                                , P.manufacturer p
                                                , OI.variantOptions oi
                                                , img
                                                , OI.lineTotal oi) : out)
             (return [])
             ois

renderOrder :: C.Splice AppHandler
renderOrder =
        C.withSplices
            C.runChildren
            (do "email"               ## pt $ (\o -> O.email o)
                "order_id"            ## pt $ (\o -> DT.pack . show . O.orderId $ o)
                "billing_address"     ## \_ -> renderAddress (orderAddressRuntime OA.Billing)
                "shipping_address"    ## \_ -> renderAddress (orderAddressRuntime OA.Shipping)
                "items"               ## \_ -> renderItems orderItemRuntime
                "processed_time"      ## pt $ (\o -> case O.processedTime o of
                                              Nothing -> ""
                                              Just ptme -> DT.pack . show $ ptme)
                "processed_style"     ## pt $ (\o -> case O.processedTime o of
                                              Nothing -> "display: none;"
                                              Just _ -> "")
                "not_processed_style" ## pt $ (\o -> case O.processedTime o of
                                              Nothing -> ""
                                              Just _ -> "display: none;")
                "total"               ## pt $ (\o -> DT.pack . formatPrice $ (O.total o)))
            orderRuntime
    where
        pt = C.pureSplice . escapingTextSplice

orderRuntime :: RuntimeSplice AppHandler O.Order
orderRuntime = do
        oid <- lift $ getFormInt "order_id" 0
        lift $ with db $ O.order oid
