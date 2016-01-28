{-# LANGUAGE OverloadedStrings #-}

module Admin.Admin where

import Admin.Categories
import Admin.Feedback
import Admin.NewProduct
import Admin.ProductVariants
import Application
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe
import FormUtil
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist.Compiled
import Text.Printf
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString.Char8 as C8
import qualified ShopData.Order as O

handleLogin :: AppHandler ()
handleLogin = method GET handleLoginForm <|> method POST handleLoginFormSubmit
  where
    handleLoginForm = render "admin/login"
    handleLoginFormSubmit = do
        un <- getParam "username"
        pwd <- getParam "password"
        u <- with auth $ loginByUsername
                (DTE.decodeUtf8 $ fromMaybe "" un)
                (ClearText $ fromMaybe "" pwd)
                False
        case u of
            Left _ -> dangerRedirect "/admin" "Bad Username or Password"
            Right _ -> redirect "/admin"


requireAdmin :: AppHandler () -> AppHandler ()
requireAdmin = requireUser auth handleLogin

handleOrder :: Handler App App ()
handleOrder =
        method GET handleOrderGet <|>
        method POST handleOrderPost
    where
        handleOrderGet = do
            oid <- getFormInt "order_id" 0
            if oid == 0
                then redirect "/admin/"
                else render "admin/_order"
        handleOrderPost = do
            oid <- getFormInt "order_id" 0
            if oid == 0
                then redirect "/admin/"
                else do
                    with db $ O.processOrder oid
                    redirect $ C8.pack (printf "/admin/order?order_id=%d" oid)

-- | The application's routes.
adminRoutes :: [(ByteString, AppHandler ())]
adminRoutes = [ ("/admin/login"           , catchErrors $ handleLogin)
              , ("/admin"                 , catchErrors $ requireAdmin $ render "admin/_index")
              , ("/admin/order"           , catchErrors $ requireAdmin $ handleOrder)
              , ("/admin/categories"      , catchErrors $ requireAdmin handleCategories)
              , ("/admin/products"        , catchErrors $ requireAdmin $ render "admin/_products")
              , ("/admin/product_options" , catchErrors $ requireAdmin handleProductVariants)
              , ("/admin/new_product"     , catchErrors $ requireAdmin handleNewProduct)
              ]
