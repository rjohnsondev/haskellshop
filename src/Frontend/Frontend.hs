{-# LANGUAGE OverloadedStrings #-}

module Frontend.Frontend where

import Application
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Control.Monad.Trans
import Control.Applicative
import FormUtil
import Admin.Feedback
import Text.Printf
import Basket
import qualified Data.Time.Clock as DTC
import qualified ShopData.Product as P
import qualified ShopData.Order as O
import qualified ShopData.OrderItem as OI
import qualified ShopData.OrderAddress as OA
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8


handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

handleHomepage :: AppHandler ()
handleHomepage = render "index"

handleProduct :: AppHandler ()
handleProduct = method GET handleProductGet <|> method POST handleProductPost
  where
    handleProductGet = cRender "product"
    handleProductPost = do
        pid <- getFormInt "product_id" 0
        c <- getFormInt "c" 0
        pvos <- getFormIntArray "variant_options"
        b <- basket
        saveBasket $ addToBasket pid (S.fromList pvos) b
        info "Item added to your basket!\
             \<div class=\"pull-right\"><a href=\"/basket\">Checkout Now \
             \<i class=\"glyphicon glyphicon-chevron-right\"></i>\
             \</a></div>"
        redirect $ C8.pack $ printf "/product?c=%d&product_id=%d" c pid


handleBasketAdjust :: Bool -> AppHandler ()
handleBasketAdjust incr =
        if incr
            then do
                key <- getFormByteString "basket-increase" ""
                b <- basket
                saveBasket $ uncurry addToBasket (deSerializeBasketKey . DTE.decodeUtf8 $ key) b
                redirect "/basket"
            else do
                key <- getFormByteString "basket-decrease" ""
                b <- basket
                saveBasket $ uncurry reduceFromBasket (deSerializeBasketKey . DTE.decodeUtf8 $ key) b
                redirect "/basket"


handleBasket :: AppHandler ()
handleBasket =
        method GET handleBasketGet <|>
        method POST handleBasketPost
  where
    handleBasketGet = cRender "basket"
    handler p
        | "basket-increase" `M.member` p = handleBasketAdjust True
        | "basket-decrease" `M.member` p = handleBasketAdjust False
        | otherwise                      = redirect "/basket"
    handleBasketPost = do
        params <- getParams
        handler params

setSessionFromForm :: BS.ByteString -> AppHandler ()
setSessionFromForm field = do
        fv <- getFormByteString field ""
        with sess $ setInSession (DTE.decodeUtf8 field) (DTE.decodeUtf8 fv)
        return ()

validateFields :: [BS.ByteString] -> AppHandler Bool
validateFields fields = 
        foldr (\field out' -> do
                  out <- out'
                  fv <- getFormByteString field ""
                  if (fv == "")
                      then do with sess $ setInSession (DTE.decodeUtf8 $ BS.concat [field, "_class"]) "has-error"
                              return False
                      else do with sess $ deleteFromSession (DTE.decodeUtf8 $ BS.concat [field, "_class"])
                              return out)
            (return True)
            fields

billingAddress :: AppHandler OA.OrderAddress
billingAddress = do
        name     <- get "billing_full_name"
        line1    <- get "billing_address_1"
        line2    <- get "billing_address_2"
        city     <- get "billing_city"
        state    <- get "billing_state"
        postcode <- get "billing_postcode"
        country  <- get "billing_country"
        return $ OA.OrderAddress 0
                                 0
                                 OA.Billing
                                 name
                                 line1
                                 line2
                                 city
                                 state
                                 postcode
                                 country
    where
        get x = do v <- getFormByteString x ""
                   return $ DTE.decodeUtf8 v

shippingAddress :: AppHandler OA.OrderAddress
shippingAddress = do
        name     <- get "shipping_full_name"
        line1    <- get "shipping_address_1"
        line2    <- get "shipping_address_2"
        city     <- get "shipping_city"
        state    <- get "shipping_state"
        postcode <- get "shipping_postcode"
        country  <- get "shipping_country"
        return $ OA.OrderAddress 0
                                 0
                                 OA.Shipping
                                 name
                                 line1
                                 line2
                                 city
                                 state
                                 postcode
                                 country
    where
        get x = do v <- getFormByteString x ""
                   return $ DTE.decodeUtf8 v

handlePaymentPost :: AppHandler ()
handlePaymentPost = do
    -- do some form validation and add values to the session
    let fields = ["shipping_full_name", "billing_full_name",
                  "shipping_address_1", "billing_address_1",
                  "shipping_address_2", "billing_address_2",
                  "shipping_city", "billing_city", "shipping_state",
                  "billing_state", "shipping_postcode",
                  "billing_postcode", "shipping_country",
                  "billing_country", "email", "phone"] :: [BS.ByteString]
    mapM_ setSessionFromForm fields
    let req_fields = ["billing_full_name", "billing_address_1",
                      "billing_city",  "billing_country",
                      "shipping_full_name", "shipping_address_1",
                      "shipping_city", "shipping_country", "email",
                      "phone"]
    isValid <- validateFields req_fields
    if isValid == False
        then do
            dangerRedirect "/payment" "Please complete all required fields"
        else do
            now <- liftIO $ DTC.getCurrentTime
            email <- getFormByteString "email" ""
            phone <- getFormByteString "phone" ""
            bvms <- basketViewModels
            let total = foldr (\(_, _, t, _, _, _) out -> t + out) 0 bvms
            let o = O.Order 0 now total (DTE.decodeUtf8 email) (DTE.decodeUtf8 phone) Nothing
            let ois = map (\(_, p, lt, q, pvost, _) -> OI.OrderItem 0 0 (P.productId p) pvost lt q) bvms
            billing <- billingAddress
            shipping <- shippingAddress
            oid <- with db $ O.placeOrder o ois [billing, shipping]
            with sess $ mapM_ deleteFromSession (map DTE.decodeUtf8 fields)
            with sess $ deleteFromSession "basket"
            with sess commitSession
            redirect $ C8.concat ["/order_confirmation?order_id=", C8.pack $ show oid]

handlePayment :: AppHandler ()
handlePayment =
        method GET handlePaymentGet <|>
        method POST handlePaymentPost
  where
    handlePaymentGet = do
        b <- basket
        if null b then redirect "/basket"
                  else cRender "payment"

frontendRoutes :: [(BS.ByteString, AppHandler ())]
frontendRoutes = [ ("/logout", catchErrors $ with auth handleLogout)
                 , ("/product", catchErrors $ handleProduct)
                 , ("/basket", catchErrors $ handleBasket)
                 , ("/payment", catchErrors $ handlePayment)
                 ]
