{-# LANGUAGE OverloadedStrings #-}

module ShopData.ProductDetail where

import qualified Snap.Snaplet.PostgresqlSimple as PS
import Database.PostgreSQL.Simple
import Control.Monad
import qualified Data.Text as DT


data ProductDetail = ProductDetail
    {   productDetailId :: Int,
        productId       :: Int,
        title           :: DT.Text,
        detail          :: DT.Text
    } deriving (Show)

instance FromRow ProductDetail where
        fromRow = ProductDetail <$> PS.field <*> PS.field <*> PS.field <*> PS.field

saveProductDetail :: PS.HasPostgres m => ProductDetail -> m Int
saveProductDetail pd = do
        r <- if (productDetailId pd) == 0
                 then PS.query
                         "INSERT INTO product_details (product_id, title, detail) \
                         \ VALUES (?, ?, ?) RETURNING product_detail_id"
                         (productId pd, title pd, detail pd)
                         :: PS.HasPostgres s => s [[Int]]
                 else PS.query
                         "UPDATE product_details SET\
                         \     title = ?, \
                         \     detail = ? \
                         \ WHERE \
                         \     product_detail_id = ? \
                         \ RETURNING product_detail_id"
                         (title pd, detail pd, productDetailId pd)
                         :: PS.HasPostgres s => s [[Int]]
        return $ head $ head r

delProductDetail :: PS.HasPostgres m => Int -> m ()
delProductDetail pdId = do
        PS.execute "DELETE FROM product_details WHERE product_detail_id = ?" (Only pdId)
        return ()

moveProductDetail :: PS.HasPostgres m => Bool -> Int -> Int -> m ()
moveProductDetail down pid did = PS.withTransaction $ do
    r <- PS.query "SELECT order_by FROM product_details WHERE product_detail_id = ?" (Only did)
            :: PS.HasPostgres m => m [[Int]]
    unless (null r) $ do
        let o = head $ head r
        results <- if down then PS.query "SELECT \
                                         \    product_detail_id, \
                                         \    order_by \
                                         \FROM \
                                         \    product_details \
                                         \WHERE \
                                         \    product_id = ? AND \
                                         \    order_by > ? \
                                         \ORDER BY order_by ASC LIMIT 1" [pid, o]
                                         :: PS.HasPostgres m => m [(Int, Int)]
                           else PS.query "SELECT \
                                         \    product_detail_id, \
                                         \    order_by \
                                         \FROM \
                                         \    product_details \
                                         \WHERE \
                                         \    product_id = ? AND \
                                         \    order_by < ? \
                                         \ORDER BY order_by DESC LIMIT 1" [pid, o]
                                         :: PS.HasPostgres m => m [(Int, Int)]
        unless (null results) $ do
            let (did', o') = head results :: (Int, Int)
            PS.execute
                "UPDATE product_details SET order_by = ? WHERE product_id = ? AND product_detail_id = ?"
                (o', pid, did)
            PS.execute
                "UPDATE product_details SET order_by = ? WHERE product_id = ? AND product_detail_id = ?"
                (o, pid, did')
            return ()
