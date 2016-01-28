{-# LANGUAGE OverloadedStrings #-}


------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------

import Application
import Control.Lens
import Data.ByteString.Char8 as C8
import Heist
import Snap
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.PostgresqlSimple
import Snap.Snaplet.Heist.Compiled
import Snap.Snaplet.PostgresqlSimple
import Snap.Snaplet.Session
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import qualified Admin.Admin as A
import qualified Frontend.Frontend as F
import qualified Heist.Compiled as C
import Render.Alerts
import Render.Variants
import Render.Product
import Render.Products
import Render.Categories
import Render.Basket
import Render.Payment
import Render.Order
import Render.Orders


allSplices :: Splices (C.Splice AppHandler)
allSplices = (do "alerts"                    ## renderAlerts alertRuntime
                 "variants"                  ## renderVariants variantsRuntime
                 "variants_url_selected"     ## renderVariants variantsRuntimeUrl
                 "variants_filtered"         ## renderVariants variantsRuntimeActive -- used in front-end
                 "admin_category_tree"       ## renderCategoriesCompiled
                 "category_list"             ## renderCategoryList categoryListRuntime
                 "product_list"              ## renderProductList productListFilteredRuntime
                 "product_list_unfiltered"   ## renderProductList productListRuntime
                 "product"                   ## renderProduct productRuntime
                 "top_level_categories"      ## renderCategories categoriesTopLevelRuntime
                 "categories"                ## renderCategories categoriesRuntime
                 "category"                  ## renderCategory categoryRuntime
                 "hide_categories"           ## renderHideCategories categoriesRuntime
                 "category_crumbtrail"       ## renderCategories categoryCrumbtrailRuntime
                 "basket"                    ## renderBasket
                 "payment"                   ## renderPayment
                 "order"                     ## renderOrder
                 "price_filter"              ## renderPriceFilter priceFilterRuntime
                 "orders_date_range_options" ## C.pureSplice (C.nodeSplice dateRangeOptionsList) dateRangeOptionsListRuntime
                 "orders"                    ## renderOrderList ordersRuntime)

routes :: [(ByteString, Handler App App ())]
routes = Prelude.concat [A.adminRoutes,
                         F.frontendRoutes,
                         [ ("/static/", serveDirectory "static")
                         , ("/favicon.ico", serveFile "static/favicon.ico")]]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addConfig h (mempty & scCompiledSplices .~ allSplices)
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    addRoutes routes
    wrapSite $ withSession sess
    addAuthSplices h auth
    return $ App h s a d

