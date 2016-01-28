{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Render.Orders where

import Application
import Control.Monad.Trans
import FormUtil
import Heist
import Heist.Compiled as C
import HeistUtil
import Snap.Snaplet
import Text.XmlHtml
import qualified Data.Text as DT
import qualified Data.Time.Clock as DTC
import qualified ShopData.Order as O

renderOrderListItem :: Splices (RuntimeSplice AppHandler O.Order -> C.Splice AppHandler)
renderOrderListItem = do
        "order_id"          ## pt $ (\o -> DT.pack . show . O.orderId $ o)
        "order_email"       ## pt $ (\o -> O.email o)
        "order_processed"   ## pt $ (\o ->  case O.processedTime o of
                                                Nothing -> "Pending"
                                                Just ptme -> DT.concat [ "Processed: "
                                                                     , DT.pack . show $ ptme])
        "order_total"      ## pt $ (\o -> DT.pack . formatPrice $ O.total o)
    where
        pt = C.pureSplice . escapingTextSplice


dateRangeOptionsListRuntime :: RuntimeSplice AppHandler Int
dateRangeOptionsListRuntime = lift $ getFormInt "orders_range" 0

dateRangeOptionsList :: Int -> [Node]
dateRangeOptionsList selected =
        map (\(k, v) -> Element { elementTag = "option",
                                  elementAttrs =
                                    (if k == selected
                                        then [("selected", ""), ("value", DT.pack . show $ k)]
                                        else [("value", DT.pack . show $ k)]),
                                  elementChildren = [TextNode v]})
       [ (0, "Pending Orders")
       , (1, "Processed Last 7 days")
       , (2, "Processed 8 to 14 days ago")
       , (3, "Processed 15 to 31 days ago")
       , (4, "Processed > 31 days ago")]

ordersRuntime :: RuntimeSplice AppHandler [O.Order]
ordersRuntime = do
        range <- lift $ getFormInt "orders_range" 0
        now <- liftIO $ DTC.getCurrentTime
        lift $ with db $ O.orders $ (O.dateRanges now) !! range

renderOrderList :: RuntimeSplice AppHandler [O.Order] -> C.Splice AppHandler
renderOrderList d = C.manyWithSplices C.runChildren renderOrderListItem d

