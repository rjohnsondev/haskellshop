{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ShopData.OrderItem where

import Database.PostgreSQL.Simple
import Data.Scientific
import qualified Data.Text as DT
import qualified Snap.Snaplet.PostgresqlSimple as PS

data OrderItem = OrderItem
    {   orderItemId    :: Int,
        orderId        :: Int,
        productId      :: Int,
        variantOptions :: DT.Text,
        lineTotal      :: Scientific,
        quantity       :: Int
    } deriving (Show)

instance FromRow OrderItem where
        fromRow = OrderItem <$> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field

saveOrderItem :: PS.HasPostgres m => OrderItem -> m Int
saveOrderItem oi = do
    ids :: [[Int]] <- PS.query
        " INSERT INTO order_items (order_id, product_id, variant_options, line_total, quantity) \
        \ VALUES (?,?,?,?,?) RETURNING order_item_id"
        (orderId oi, productId oi, variantOptions oi, lineTotal oi, quantity oi)
    return $ head $ head ids

orderItems :: PS.HasPostgres m => Int -> m [OrderItem]
orderItems oid =
        PS.query " SELECT\
                 \   order_item_id,\
                 \   order_id,\
                 \   product_id,\
                 \   variant_options,\
                 \   line_total,\
                 \   quantity\
                 \ FROM\
                 \   order_items\
                 \ WHERE\
                 \   order_id = ?"
                 (Only oid)

        
