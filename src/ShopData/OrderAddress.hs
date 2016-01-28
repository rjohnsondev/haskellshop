{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module ShopData.OrderAddress where

import Database.PostgreSQL.Simple
import qualified Data.Text as DT
import qualified Snap.Snaplet.PostgresqlSimple as PS
import Database.PostgreSQL.Simple.ToField

import Database.PostgreSQL.Simple.FromField
       ( FromField (fromField), typeOid, returnError)
import Database.PostgreSQL.Simple.TypeInfo.Static (typoid, varchar)
import qualified Data.ByteString.Char8 as C8


data AddressType = Billing | Shipping
           deriving (Read, Show)

instance FromField AddressType where
   fromField f mdata =
      if typeOid f /= typoid varchar
        then returnError Incompatible f ""
        else case C8.unpack `fmap` mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat ->
                  case [ x | (x,t) <- reads dat, ("","") <- lex t ] of
                    [x] -> return x
                    _   -> returnError ConversionFailed f dat

instance ToField AddressType where
    toField x = toField $ show $ x

data OrderAddress = OrderAddress
    {   orderAddressId :: Int,
        orderId        :: Int,
        addressType    :: AddressType,
        fullName       :: DT.Text,
        line1          :: DT.Text,
        line2          :: DT.Text,
        city           :: DT.Text,
        state          :: DT.Text,
        postcode       :: DT.Text,
        country        :: DT.Text
    } deriving (Show)

instance FromRow OrderAddress where
        fromRow = OrderAddress <$> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field
                               <*> PS.field <*> PS.field <*> PS.field <*> PS.field <*> PS.field

orderAddress :: PS.HasPostgres m => Int -> AddressType -> m OrderAddress
orderAddress oid at = do
        results :: [OrderAddress] <- PS.query " SELECT \
                                              \   order_address_id,\
                                              \   order_id,\
                                              \   address_type,\
                                              \   full_name,\
                                              \   line_1,\
                                              \   line_2,\
                                              \   city,\
                                              \   state,\
                                              \   postcode,\
                                              \   country\
                                              \ FROM\
                                              \   order_addresses\
                                              \ WHERE\
                                              \   order_id = ? AND\
                                              \   address_type = ?"
                                              (oid, at)
        return $ head results

saveOrderAddress :: PS.HasPostgres m => OrderAddress -> m Int
saveOrderAddress oa = do
        ids :: [[Int]] <- PS.query
            " INSERT INTO order_addresses (\
            \   order_id,\
            \   address_type,\
            \   full_name,\
            \   line_1,\
            \   line_2,\
            \   city,\
            \   state,\
            \   postcode,\
            \   country)\
            \ VALUES\
            \   (?, ?, ?, ?, ?, ?, ?, ?, ?)\
            \ RETURNING order_address_id"
            (orderId oa, (DT.pack $ show $ addressType oa), fullName oa,
             line1 oa, line2 oa, city oa, state oa,
             postcode oa, country oa)
        return $ head $ head $ ids

