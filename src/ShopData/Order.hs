{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ShopData.Order where

import Database.PostgreSQL.Simple
import Data.Scientific
import Control.Monad.Trans
import qualified Data.Time.Calendar as DTCal
import qualified Data.Time.Clock as DTC
import qualified Data.Text as DT
import qualified Snap.Snaplet.PostgresqlSimple as PS
import qualified ShopData.OrderItem as OI
import qualified ShopData.OrderAddress as OA

data Order = Order
    {   orderId       :: Int,
        purchaseTime  :: DTC.UTCTime,
        total         :: Scientific,
        email         :: DT.Text,
        phone         :: DT.Text,
        processedTime :: Maybe DTC.UTCTime
    } deriving (Show)

instance FromRow Order where
        fromRow = Order <$> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field

placeOrder :: PS.HasPostgres m => Order -> [OI.OrderItem] -> [OA.OrderAddress] -> m Int
placeOrder o ois oas = PS.withTransaction $ do
    ids :: [[Int]] <- PS.query
        "INSERT INTO orders (purchase_time, total, email, phone) VALUES (?, ?, ?, ?) RETURNING order_id"
        (purchaseTime o, total o, email o, phone o)
    let oid = head $ head ids
    mapM_ (\oi -> OI.saveOrderItem $ oi {OI.orderId = oid}) ois
    mapM_ (\oa -> OA.saveOrderAddress $ oa {OA.orderId = oid}) oas
    return oid


processOrder :: PS.HasPostgres m => Int -> m ()
processOrder oid = do
        now <- liftIO $ DTC.getCurrentTime
        PS.execute "UPDATE orders SET processed_time = ? WHERE order_id = ? AND processed_time IS NULL" (now, oid)
        return ()

dateRanges :: DTC.UTCTime -> [Maybe (DTC.UTCTime, Maybe DTC.UTCTime)]
dateRanges now =
        [ Nothing
        , Just (addDays now (-7), Just now)
        , Just (addDays now (-14), Just $ addDays now (-8))
        , Just (addDays now (-31), Just $ addDays now (-15))
        , Just (addDays now (-32), Nothing) ]
    where
        addDays t diff = DTC.UTCTime (DTCal.addDays diff (DTC.utctDay t)) (DTC.secondsToDiffTime 0)

order :: PS.HasPostgres m => Int -> m Order
order oid = do
        o :: [Order] <- PS.query " SELECT\
                                 \     order_id,\
                                 \     purchase_time,\
                                 \     total,\
                                 \     email,\
                                 \     phone,\
                                 \     processed_time\
                                 \ FROM\
                                 \     orders\
                                 \ WHERE\
                                 \     order_id = ?" (Only oid)
        return $ head o

orders :: PS.HasPostgres m => Maybe (DTC.UTCTime, Maybe DTC.UTCTime) -> m [Order]
orders pt = 
        case pt of
            Nothing -> PS.query_ " SELECT\
                                 \     order_id,\
                                 \     purchase_time,\
                                 \     total,\
                                 \     email,\
                                 \     phone,\
                                 \     processed_time\
                                 \ FROM\
                                 \     orders\
                                 \ WHERE\
                                 \     processed_time IS NULL"
            Just (start, end) ->
                case end of
                    Nothing -> PS.query " SELECT\
                                        \     order_id,\
                                        \     purchase_time,\
                                        \     total,\
                                        \     email,\
                                        \     phone,\
                                        \     processed_time\
                                        \ FROM\
                                        \     orders\
                                        \ WHERE\
                                        \     processed_time < ?"
                                        (Only start)
                    Just e -> PS.query " SELECT\
                                       \     order_id,\
                                       \     purchase_time,\
                                       \     total,\
                                       \     email,\
                                       \     phone,\
                                       \     processed_time\
                                       \ FROM\
                                       \     orders\
                                       \ WHERE\
                                       \     processed_time BETWEEN ? AND ?"
                                       (start, e)


