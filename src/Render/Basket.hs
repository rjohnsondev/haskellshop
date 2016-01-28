{-# LANGUAGE OverloadedStrings #-}

module Render.Basket where

import Application
import Heist
import HeistUtil
import Control.Monad.Trans
import Basket
import qualified ShopData.Product as P
import qualified Heist.Compiled as C
import qualified Data.Text as DT

renderBasketCount :: RuntimeSplice AppHandler [BasketViewModel] -> C.Splice AppHandler
renderBasketCount = 
        C.pureSplice . escapingTextSplice $ DT.pack . show . total
    where
        total = foldr (\(_, _, _, quant, _, _) out -> out + quant) 0

renderBasketItem :: Splices (RuntimeSplice AppHandler BasketViewModel -> C.Splice AppHandler)
renderBasketItem = do
        "product_basket_key"   ## pt $ (\(k, _, _, _, _, _) -> k)
        "product_name"         ## pt $ (\(_, p, _, _, _, _) -> P.name p)
        "product_hash"         ## pt $ (\(_, _, _, _, _, ph) -> ph)
        "product_manufacturer" ## pt $ (\(_, p, _, _, _, _) -> P.manufacturer p)
        "product_options"      ## pt $ (\(_, _, _, _, h, _) -> h)
        "product_price"        ## pt $ (\(_, _, pr, _, _, _) -> DT.pack . formatPrice $ pr)
        "product_quantity"     ## pt $ (\(_, _, _, c, _, _) -> DT.pack . show $ c)
    where
        pt = C.pureSplice . escapingTextSplice

renderBasketItems :: RuntimeSplice AppHandler [BasketViewModel] -> C.Splice AppHandler
renderBasketItems = C.manyWithSplices C.runChildren renderBasketItem

renderBasket :: C.Splice AppHandler
renderBasket =
        C.withSplices
            C.runChildren
            (do "basket_count"           ## renderBasketCount
                "basket_items"           ## renderBasketItems
                "basket_total"           ## renderBasketTotal
                "empty_basket_class"     ## pt $ (\x -> if length x > 0 then "display: none" else "")
                "non_empty_basket_class" ## pt $ (\x -> if length x > 0 then "" else "display: none"))
            basketRuntime
    where
        pt = C.pureSplice . escapingTextSplice


renderBasketTotal :: RuntimeSplice AppHandler [BasketViewModel] -> C.Splice AppHandler
renderBasketTotal = 
        C.pureSplice . escapingTextSplice $ DT.pack . formatPrice . total
    where
        total = foldr (\(_, _, pr, _, _, _) out -> out + pr) 0.0

basketRuntime :: RuntimeSplice AppHandler [BasketViewModel]
basketRuntime = lift basketViewModels
